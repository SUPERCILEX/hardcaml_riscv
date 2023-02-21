open! Core
open Hardcaml
module Uart = Uart
module Bootloader = Bootloader

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; uart : 'a Uart.I.t [@rtlmangle true]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { error : 'a
    ; uart : 'a Uart.O.t [@rtlmangle true]
    }
  [@@deriving sexp_of, hardcaml]
end

module State = struct
  type t =
    | Fetch
    | Decode_and_load
    | Execute
    | Writeback
    | Error
  [@@deriving sexp_of, compare, enumerate]
end

let do_on_load instruction ~s =
  Instruction.Binary.Of_always.match_
    ~default:[]
    instruction
    (Instruction.RV32I.[ Lb; Lbu; Lh; Lhu; Lw ] |> List.map ~f:(fun i -> i, s))
;;

let do_on_store instruction ~s =
  Instruction.Binary.Of_always.match_
    ~default:[]
    instruction
    (Instruction.RV32I.[ Sb; Sh; Sw ] |> List.map ~f:(fun i -> i, s))
;;

module Fetch_stage = struct
  type 'a t = { load_instruction : 'a } [@@deriving sexp_of, hardcaml]
end

let fetch ~(sm : State.t Always.State_machine.t) { Fetch_stage.load_instruction } =
  let open Signal in
  Always.[ load_instruction <-- vdd; sm.set_next Decode_and_load ]
;;

module Load_mem_stage = struct
  type ('a, 'b) t =
    { instruction : 'a Instruction.Binary.t
    ; load : 'b
    ; data_size : 'b Memory_controller.Size.Binary.t
    }
end

let load_mem { Load_mem_stage.instruction; load; data_size } =
  let open Signal in
  Always.
    [ do_on_load instruction ~s:[ load <-- vdd ]
    ; Instruction.Binary.Of_always.match_
        ~default:[]
        instruction
        ([ [ Instruction.RV32I.Lb; Lbu ], Memory_controller.Size.Enum.Byte
         ; [ Lh; Lhu ], Half_word
         ; [ Lw ], Word
         ]
        |> List.map ~f:(fun (instructions, s) -> List.map instructions ~f:(fun i -> i, s))
        |> List.concat
        |> List.map ~f:(fun (i, s) ->
             ( i
             , [ Memory_controller.Size.Binary.(
                   Of_always.assign data_size (Of_signal.of_enum s))
               ] )))
    ]
;;

let execute ~(sm : State.t Always.State_machine.t) = [ sm.set_next Writeback ]

module Writeback_stage = struct
  type ('a, 'b) t =
    { instruction : 'a Instruction.Binary.t
    ; jump : 'a
    ; jump_target : 'a
    ; store : 'b
    ; program_counter : 'b
    ; data_size : 'b Memory_controller.Size.Binary.t
    }
end

let writeback
  ~(sm : State.t Always.State_machine.t)
  { Writeback_stage.instruction; jump; jump_target; store; program_counter; data_size }
  =
  let open Signal in
  Always.
    [ if_
        jump
        [ program_counter <-- jump_target ]
        [ program_counter <-- program_counter.value +:. 4 ]
    ; do_on_store instruction ~s:[ store <-- vdd ]
    ; Instruction.Binary.Of_always.match_
        ~default:[]
        instruction
        ([ Instruction.RV32I.Sb, Memory_controller.Size.Enum.Byte
         ; Sh, Half_word
         ; Sw, Word
         ]
        |> List.map ~f:(fun (i, s) ->
             ( i
             , [ Memory_controller.Size.Binary.(
                   Of_always.assign data_size (Of_signal.of_enum s))
               ] )))
    ; sm.set_next Fetch
    ]
;;

let create scope ~bootloader { I.clock; clear; uart } =
  let open Signal in
  let spec = Reg_spec.create ~clock ~clear () in
  let stall = wire 1 in
  let sm = Always.State_machine.create ~enable:~:stall (module State) spec in
  let memory_controller =
    { (Memory_controller.I.Of_always.wire zero) with
      program_counter =
        Always.Variable.reg
          ~enable:~:stall
          ~width:Parameters.word_size
          (Reg_spec.override
             spec
             ~clear_to:(of_int ~width:Parameters.word_size Parameters.bootloader_start))
    }
  in
  let { Memory_controller.O.instruction = raw_instruction
      ; read_data = memory_read_data
      ; error = invalid_address
      ; uart = uart_out
      ; stall = mem_stall
      }
    =
    Memory_controller.circuit
      scope
      { (Memory_controller.I.Of_always.value memory_controller) with clock; uart }
      ~bootloader:(String.to_list bootloader |> List.map ~f:Signal.of_char)
  in
  stall <== mem_stall;
  let decoder_raw = Decoder.circuit scope { Decoder.I.instruction = raw_instruction } in
  let ({ Decoder.O.instruction; immediate; _ } as decoder) =
    decoder_raw |> Decoder.O.map ~f:(reg ~enable:(sm.is Decode_and_load) spec)
  in
  let alu_feedback = Alu.O.Of_signal.wires () in
  let { Register_file.O.rs1; rs2 } =
    let { Alu.O.store = reg_store; rd = alu_result; _ } = alu_feedback in
    let { Decoder.O.rs1; rs2; _ } = decoder_raw in
    Register_file.circuit
      scope
      { Register_file.I.clock
      ; write_address = decoder.rd
      ; read_address1 = rs1
      ; read_address2 = rs2
      ; store = reg_store &: sm.is Writeback
      ; load = sm.is Decode_and_load
      ; write_data =
          Instruction.Binary.Of_signal.match_
            ~default:alu_result
            instruction
            (Instruction.RV32I.[ Lb; Lbu; Lh; Lhu; Lw ]
            |> List.map ~f:(fun i -> i, memory_read_data))
      }
  in
  memory_controller.data_address.value <== rs1 +: immediate;
  memory_controller.write_data.value <== rs2;
  let alu =
    Alu.circuit
      scope
      { Alu.I.pc = memory_controller.program_counter.value
      ; instruction
      ; rs1
      ; rs2
      ; immediate
      }
    |> Alu.O.map ~f:(reg ~enable:(sm.is Execute) spec)
  in
  Alu.O.iter2 alu_feedback alu ~f:( <== );
  let _debugging =
    let ( -- ) = Scope.naming scope in
    ignore (sm.current -- "state")
  in
  Always.(
    compile
      [ sm.switch
          [ ( Fetch
            , fetch
                ~sm
                { Fetch_stage.load_instruction = memory_controller.load_instruction } )
          ; Decode_and_load, [ sm.set_next Execute ]
          ; ( Execute
            , load_mem
                { Load_mem_stage.instruction
                ; load = memory_controller.load
                ; data_size = memory_controller.data_size
                }
              @ execute ~sm
              @ [ when_
                    (Instruction.Binary.Of_signal.is instruction Invalid)
                    [ sm.set_next Error ]
                ] )
          ; ( Writeback
            , writeback
                ~sm
                { Writeback_stage.instruction
                ; jump = alu.jump
                ; jump_target = alu.jump_target
                ; store = memory_controller.store
                ; program_counter = memory_controller.program_counter
                ; data_size = memory_controller.data_size
                } )
          ; Error, []
          ]
      ; when_ invalid_address [ sm.set_next Error ]
      ]);
  { O.error = sm.is Error; uart = uart_out }
;;

let circuit scope =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"cpu" (create ~bootloader:Bootloader.bytes)
;;

module Tests = struct
  module Simulator = Cyclesim.With_interface (I) (O)

  let test_bench (sim : (_ I.t, _ O.t) Cyclesim.t) ~step ~uart_data =
    let open Bits in
    let inputs, outputs = Cyclesim.inputs sim, Cyclesim.outputs sim in
    let reset () =
      Cyclesim.reset sim;
      inputs.clear := vdd;
      Cyclesim.cycle sim;
      inputs.clear := gnd
    in
    reset ();
    let rec run i uart_data =
      let read_done = i % 11 = 0 in
      let write_done = i % 17 = 0 in
      inputs.uart.read_done := if read_done then vdd else gnd;
      inputs.uart.write_done := if write_done then vdd else gnd;
      inputs.uart.read_data
        := (if read_done then List.hd uart_data else None)
           |> Option.value ~default:(Char.of_int_exn 0)
           |> of_char;
      if write_done && to_bool !(outputs.uart.write_ready)
      then
        Stdio.print_string
          (to_int !(outputs.uart.write_data) |> Char.of_int_exn |> String.of_char)
      else ();
      Cyclesim.cycle sim;
      if step i
      then ()
      else run (i + 1) (if read_done then List.drop uart_data 1 else uart_data)
    in
    run 1 uart_data
  ;;

  let prettify_enum ~sim ~(enums : 'a list) ~signal_name : 'a =
    let open Bits in
    List.nth_exn
      enums
      (to_int
         !(List.Assoc.find_exn
             (Cyclesim.internal_ports sim)
             signal_name
             ~equal:String.equal))
  ;;

  let sim ~program ?uart_data termination =
    let scope = Scope.create ~flatten_design:true () in
    let sim =
      Simulator.create
        ~config:Cyclesim.Config.trace_all
        (create scope ~bootloader:program)
    in
    let inputs, outputs = Cyclesim.inputs sim, Cyclesim.outputs sim in
    test_bench sim ~uart_data:(Option.value uart_data ~default:[]) ~step:(fun i ->
      let open Bits in
      let all_signals =
        Cyclesim.internal_ports sim
        |> List.filter_map ~f:(fun (signal_name, signal) ->
             let prettify_enum enums = prettify_enum ~sim ~signal_name ~enums in
             let is_signal = String.equal signal_name in
             (if is_signal "state"
             then prettify_enum State.all |> State.sexp_of_t |> Some
             else if is_signal "alu$binary_variant"
             then Instruction.(prettify_enum RV32I.all |> RV32I.sexp_of_t) |> Some
             else if String.is_prefix ~prefix:"alu" signal_name
                     || String.is_prefix ~prefix:"register_file" signal_name
             then to_int !signal |> Printf.sprintf "0x%x" |> String.sexp_of_t |> Some
             else None)
             |> Option.map ~f:(fun s -> signal_name, Sexp.to_string s))
        |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
      in
      Stdio.print_s
        ([%sexp_of: State.t * int I.t * int O.t * (string * string) list]
           ( prettify_enum ~sim ~enums:State.all ~signal_name:"state"
           , I.map inputs ~f:(fun p -> to_int !p)
           , O.map outputs ~f:(fun p -> to_int !p)
           , all_signals ));
      termination i)
  ;;

  let waves ~program ~cycles ?uart_data f =
    let open Hardcaml_waveterm in
    let scope = Scope.create ~flatten_design:true () in
    let sim =
      Simulator.create
        ~config:Cyclesim.Config.trace_all
        (create scope ~bootloader:program)
    in
    let waves, sim = Waveform.create sim in
    test_bench sim ~step:(( = ) cycles) ~uart_data:(Option.value uart_data ~default:[]);
    let open Hardcaml_waveterm.Display_rule in
    let input_rules =
      I.(map port_names ~f:(port_name_is ~wave_format:(Bit_or Unsigned_int)) |> to_list)
    in
    let output_rules =
      O.(map port_names ~f:(port_name_is ~wave_format:(Bit_or Unsigned_int)))
    in
    let output_rules =
      (output_rules |> O.to_list)
      @ [ port_name_is
            "state"
            ~wave_format:
              (Index
                 (List.map State.all ~f:(State.sexp_of_t |> Fn.compose Sexp.to_string)))
        ; port_name_is
            "decoder$binary_variant"
            ~wave_format:
              (Index
                 Instruction.(
                   List.map RV32I.all ~f:(RV32I.sexp_of_t |> Fn.compose Sexp.to_string)))
        ; port_name_is
            "alu$binary_variant"
            ~wave_format:
              (Index
                 Instruction.(
                   List.map RV32I.all ~f:(RV32I.sexp_of_t |> Fn.compose Sexp.to_string)))
        ; port_name_is
            "memory_controller$binary_variant"
            ~wave_format:
              (Index
                 Memory_controller.Size.(
                   List.map Enum.all ~f:(Enum.sexp_of_t |> Fn.compose Sexp.to_string)))
        ]
    in
    f ~display_rules:(input_rules @ output_rules @ [ default ]) waves
  ;;
end
