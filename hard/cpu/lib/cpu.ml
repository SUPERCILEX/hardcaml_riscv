open! Core
open Hardcaml
module Uart = Uart
module Bootloader = Bootloader

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; uart : 'a Uart.I.t
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { error : 'a
    ; uart : 'a Uart.O.t
    }
  [@@deriving sexp_of, hardcaml]
end

module State = struct
  type t =
    | Fetch
    | Decode
    | Load_regs
    | Load_mem
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
  Always.[ load_instruction <-- vdd; sm.set_next Decode ]
;;

let decode ~(sm : State.t Always.State_machine.t) = [ sm.set_next Load_regs ]

module Load_regs_stage = struct
  type ('a, 'b) t =
    { instruction : 'a Instruction.Binary.t
    ; load_registers : 'b
    }
end

let load_regs
  ~(sm : State.t Always.State_machine.t)
  { Load_regs_stage.instruction; load_registers }
  =
  let open Signal in
  Always.
    [ load_registers <-- vdd
    ; if_
        (Instruction.Binary.Of_signal.is instruction Invalid)
        [ sm.set_next Error ]
        [ sm.set_next Load_mem ]
    ]
;;

module Load_mem_stage = struct
  type ('a, 'b) t =
    { instruction : 'a Instruction.Binary.t
    ; load : 'b
    ; data_size : 'b Memory_controller.Size.Binary.t
    }
end

let load_mem
  ~(sm : State.t Always.State_machine.t)
  { Load_mem_stage.instruction; load; data_size }
  =
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
    ; sm.set_next Execute
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
  let sm = Always.State_machine.create (module State) spec in
  let memory_controller =
    { (Memory_controller.I.Of_always.wire zero) with
      program_counter =
        Always.Variable.reg
          ~width:Parameters.word_size
          (Reg_spec.override
             spec
             ~clear_to:(of_int ~width:Parameters.word_size Parameters.bootloader_start))
    }
  in
  let { Memory_controller.O.instruction = raw_instruction
      ; read_data = memory_read_data
      ; error = invalid_address
      }
    =
    Memory_controller.circuit
      scope
      { (Memory_controller.I.Of_always.value memory_controller) with clock }
      ~bootloader
  in
  let decoder =
    Decoder.circuit scope { Decoder.I.instruction = raw_instruction }
    |> Decoder.O.map ~f:(reg ~enable:(sm.is Decode) spec)
  in
  let alu_feedback = Alu.O.Of_signal.wires () in
  let load_registers = Always.Variable.wire ~default:gnd in
  let { Register_file.O.rs1; rs2 } =
    let { Alu.O.store = reg_store; rd = alu_result; _ } = alu_feedback in
    let { Decoder.O.rd; rs1; rs2; _ } = decoder in
    Register_file.circuit
      scope
      { Register_file.I.clock
      ; write_address = rd
      ; read_address1 = rs1
      ; read_address2 = rs2
      ; store = reg_store &: sm.is Writeback
      ; load = load_registers.value
      ; write_data = alu_result
      }
  in
  memory_controller.data_address.value <== rs1 +: decoder.immediate;
  let alu =
    Alu.circuit
      scope
      { Alu.I.pc = memory_controller.program_counter.value
      ; data = memory_read_data
      ; instruction = decoder.instruction
      ; rs1
      ; rs2
      ; immediate = decoder.immediate
      }
    |> Alu.O.map ~f:(reg ~enable:(sm.is Execute) spec)
  in
  memory_controller.write_data.value <== alu.rd;
  Alu.O.iter2 alu_feedback alu ~f:( <== );
  let _debugging =
    let ( -- ) = Scope.naming scope in
    ignore (sm.current -- "state");
    ignore (load_registers.value -- "load_registers")
  in
  Always.(
    compile
      [ sm.switch
          [ ( Fetch
            , fetch
                ~sm
                { Fetch_stage.load_instruction = memory_controller.load_instruction } )
          ; Decode, decode ~sm
          ; ( Load_regs
            , load_regs
                ~sm
                { Load_regs_stage.instruction = decoder.instruction; load_registers } )
          ; ( Load_mem
            , load_mem
                ~sm
                { Load_mem_stage.instruction = decoder.instruction
                ; load = memory_controller.load
                ; data_size = memory_controller.data_size
                } )
          ; Execute, execute ~sm
          ; ( Writeback
            , writeback
                ~sm
                { Writeback_stage.instruction = decoder.instruction
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
  let counter = Always.Variable.reg ~width:10 spec in
  let raw_data =
    "A description of the events in figure 4 follows:\n\n\
    \  The Master puts an address on the Write Address channel and data on the Write \
     data channel. At the same time it asserts AWVALID and WVALID indicating the address \
     and data on the respective channels is valid. BREADY is also asserted by the \
     Master, indicating it is ready to receive a response.\n\
    \  The Slave asserts AWREADY and WREADY on the Write Address and Write Data \
     channels, respectively.\n\
    \  Since Valid and Ready signals are present on both the Write Address and Write \
     Data channels, the handshakes on those channels occur and the associated Valid and \
     Ready signals can be deasserted. (After both handshakes occur, the slave has the \
     write address and data)\n\
    \  The Slave asserts BVALID, indicating there is a valid reponse on the Write \
     response channel. (in this case the response is 2’b00, that being ‘OKAY’).\n\
    \  The next rising clock edge completes the transaction, with both the Ready and \
     Valid signals on the write response channel high."
    |> String.to_list
    |> List.map ~f:of_char
  in
  let is_dumping = counter.value <:. List.length raw_data in
  let data = mux counter.value raw_data |> reg spec in
  let pending_write = Always.Variable.reg ~width:1 spec in
  let pending_data = Always.Variable.reg ~width:8 spec in
  Always.(
    compile
      [ when_
          (uart.write_done &: ~:is_dumping &: pending_write.value)
          [ pending_write <-- gnd ]
      ; when_ (uart.write_done &: is_dumping) [ counter <-- counter.value +:. 1 ]
      ; when_ uart.read_done [ pending_write <-- vdd; pending_data <-- uart.read_data ]
      ]);
  { O.error = sm.is Error
  ; uart =
      { Uart.O.write_data = mux2 is_dumping data pending_data.value
      ; write_ready = is_dumping |: pending_write.value
      ; read_ready = ~:(pending_write.value)
      }
  }
;;

let circuit scope =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"cpu" (create ~bootloader:Bootloader.bytes)
;;

module Tests = struct
  module Simulator = Cyclesim.With_interface (I) (O)

  let test_bench (sim : (_ I.t, _ O.t) Cyclesim.t) ~step =
    let open Bits in
    let inputs = Cyclesim.inputs sim in
    let reset () =
      Cyclesim.reset sim;
      inputs.clear := vdd;
      Cyclesim.cycle sim;
      inputs.clear := gnd
    in
    reset ();
    let rec run i =
      Cyclesim.cycle sim;
      if step i then () else run (i + 1)
    in
    run 1
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

  let sim ~program ~termination =
    let scope = Scope.create ~flatten_design:true () in
    let sim =
      Simulator.create
        ~config:Cyclesim.Config.trace_all
        (create scope ~bootloader:program)
    in
    let inputs, outputs = Cyclesim.inputs sim, Cyclesim.outputs sim in
    test_bench sim ~step:(fun i ->
      let open Bits in
      let all_signals =
        Cyclesim.internal_ports sim
        |> List.map ~f:(fun (signal_name, signal) ->
             ( signal_name
             , let prettify_enum enums = prettify_enum ~sim ~signal_name ~enums in
               let is_signal = String.equal signal_name in
               (if is_signal "state"
               then prettify_enum State.all |> State.sexp_of_t
               else if is_signal "decoder$binary_variant"
                       || is_signal "alu$binary_variant"
               then Instruction.(prettify_enum RV32I.all |> RV32I.sexp_of_t)
               else if is_signal "memory_controller$binary_variant"
               then Memory_controller.(prettify_enum Size.Enum.all |> Size.Enum.sexp_of_t)
               else to_int !signal |> Printf.sprintf "0x%x" |> String.sexp_of_t)
               |> Sexp.to_string ))
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

  let waves ~program ~cycles f =
    let open Hardcaml_waveterm in
    let scope = Scope.create ~flatten_design:true () in
    let sim =
      Simulator.create
        ~config:Cyclesim.Config.trace_all
        (create scope ~bootloader:(Bootloader.For_testing.sample program))
    in
    let waves, sim = Waveform.create sim in
    test_bench sim ~step:(( = ) cycles);
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

  let%expect_test "Invalid program" =
    sim ~program:(Bootloader.For_testing.sample Invalid) ~termination:(( = ) 4);
    [%expect
      {|
      (Fetch
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 0) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Invalid) (alu$data 0x0) (alu$immediate 0x0)
        (alu$jump 0x0) (alu$jump_target 0x0) (alu$pc 0x4000) (alu$rd 0x0)
        (alu$rs1 0x0) (alu$rs2 0x0) (alu$store 0x0)
        (decoder$binary_variant Invalid) (decoder$immediate 0x0)
        (decoder$instruction_in 0x0) (decoder$rd 0x0) (decoder$rs1 0x0)
        (decoder$rs2 0x0) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0) (dmem_2 0x0)
        (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x0) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x0)
        (memory_controller$error 0x0) (memory_controller$instruction 0x0)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x1)
        (memory_controller$program_counter 0x4000)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0x0) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x0)
        (register_file$read_address1 0x0) (register_file$read_address2 0x0)
        (register_file$rs1 0x0) (register_file$rs2 0x0) (register_file$store 0x0)
        (register_file$write_address 0x0) (register_file$write_data 0x0)
        (state Fetch) (vdd 0x1)))
      (Decode
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 0) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Invalid) (alu$data 0x0) (alu$immediate 0x0)
        (alu$jump 0x0) (alu$jump_target 0x0) (alu$pc 0x4000) (alu$rd 0x0)
        (alu$rs1 0x0) (alu$rs2 0x0) (alu$store 0x0)
        (decoder$binary_variant Invalid) (decoder$immediate 0x0)
        (decoder$instruction_in 0x3020100) (decoder$rd 0x2) (decoder$rs1 0x4)
        (decoder$rs2 0x10) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0) (dmem_2 0x0)
        (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x0) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x0)
        (memory_controller$error 0x0) (memory_controller$instruction 0x3020100)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x0)
        (memory_controller$program_counter 0x4000)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0x0) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x0)
        (register_file$read_address1 0x0) (register_file$read_address2 0x0)
        (register_file$rs1 0x0) (register_file$rs2 0x0) (register_file$store 0x0)
        (register_file$write_address 0x0) (register_file$write_data 0x0)
        (state Decode) (vdd 0x1)))
      (Load_regs
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 1) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Invalid) (alu$data 0x0) (alu$immediate 0x0)
        (alu$jump 0x0) (alu$jump_target 0x0) (alu$pc 0x4000) (alu$rd 0x0)
        (alu$rs1 0x0) (alu$rs2 0x0) (alu$store 0x0)
        (decoder$binary_variant Invalid) (decoder$immediate 0x0)
        (decoder$instruction_in 0x3020100) (decoder$rd 0x2) (decoder$rs1 0x4)
        (decoder$rs2 0x10) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0) (dmem_2 0x0)
        (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x1) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x0)
        (memory_controller$error 0x0) (memory_controller$instruction 0x3020100)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x0)
        (memory_controller$program_counter 0x4000)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0x0) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x1)
        (register_file$read_address1 0x4) (register_file$read_address2 0x10)
        (register_file$rs1 0x0) (register_file$rs2 0x0) (register_file$store 0x0)
        (register_file$write_address 0x2) (register_file$write_data 0x0)
        (state Load_regs) (vdd 0x1)))
      (Error
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 1) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Invalid) (alu$data 0x0) (alu$immediate 0x0)
        (alu$jump 0x0) (alu$jump_target 0x0) (alu$pc 0x4000) (alu$rd 0x0)
        (alu$rs1 0x0) (alu$rs2 0x0) (alu$store 0x0)
        (decoder$binary_variant Invalid) (decoder$immediate 0x0)
        (decoder$instruction_in 0x3020100) (decoder$rd 0x2) (decoder$rs1 0x4)
        (decoder$rs2 0x10) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0) (dmem_2 0x0)
        (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x0) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x0)
        (memory_controller$error 0x0) (memory_controller$instruction 0x3020100)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x0)
        (memory_controller$program_counter 0x4000)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0x0) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x0)
        (register_file$read_address1 0x4) (register_file$read_address2 0x10)
        (register_file$rs1 0x0) (register_file$rs2 0x0) (register_file$store 0x0)
        (register_file$write_address 0x2) (register_file$write_data 0x0)
        (state Error) (vdd 0x1))) |}]
  ;;

  let%expect_test "Simple program" =
    sim ~program:(Bootloader.For_testing.sample Simple) ~termination:(( = ) 34);
    [%expect
      {|
      (Fetch
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 0) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Invalid) (alu$data 0x0) (alu$immediate 0x0)
        (alu$jump 0x0) (alu$jump_target 0x0) (alu$pc 0x4000) (alu$rd 0x0)
        (alu$rs1 0x0) (alu$rs2 0x0) (alu$store 0x0)
        (decoder$binary_variant Invalid) (decoder$immediate 0x0)
        (decoder$instruction_in 0x0) (decoder$rd 0x0) (decoder$rs1 0x0)
        (decoder$rs2 0x0) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0) (dmem_2 0x0)
        (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x0) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x0)
        (memory_controller$error 0x0) (memory_controller$instruction 0x0)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x1)
        (memory_controller$program_counter 0x4000)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0x0) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x0)
        (register_file$read_address1 0x0) (register_file$read_address2 0x0)
        (register_file$rs1 0x0) (register_file$rs2 0x0) (register_file$store 0x0)
        (register_file$write_address 0x0) (register_file$write_data 0x0)
        (state Fetch) (vdd 0x1)))
      (Decode
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 0) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Invalid) (alu$data 0x0) (alu$immediate 0x0)
        (alu$jump 0x0) (alu$jump_target 0x0) (alu$pc 0x4000) (alu$rd 0x0)
        (alu$rs1 0x0) (alu$rs2 0x0) (alu$store 0x0) (decoder$binary_variant Addi)
        (decoder$immediate 0x1) (decoder$instruction_in 0x100013) (decoder$rd 0x0)
        (decoder$rs1 0x0) (decoder$rs2 0x1) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0)
        (dmem_2 0x0) (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x0) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x0)
        (memory_controller$error 0x0) (memory_controller$instruction 0x100013)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x0)
        (memory_controller$program_counter 0x4000)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0x0) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x0)
        (register_file$read_address1 0x0) (register_file$read_address2 0x0)
        (register_file$rs1 0x0) (register_file$rs2 0x0) (register_file$store 0x0)
        (register_file$write_address 0x0) (register_file$write_data 0x0)
        (state Decode) (vdd 0x1)))
      (Load_regs
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 0) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Addi) (alu$data 0x0) (alu$immediate 0x1) (alu$jump 0x0)
        (alu$jump_target 0x0) (alu$pc 0x4000) (alu$rd 0x1) (alu$rs1 0x0)
        (alu$rs2 0x0) (alu$store 0x1) (decoder$binary_variant Addi)
        (decoder$immediate 0x1) (decoder$instruction_in 0x100013) (decoder$rd 0x0)
        (decoder$rs1 0x0) (decoder$rs2 0x1) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0)
        (dmem_2 0x0) (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x1) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x1)
        (memory_controller$error 0x0) (memory_controller$instruction 0x100013)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x0)
        (memory_controller$program_counter 0x4000)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0x0) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x1)
        (register_file$read_address1 0x0) (register_file$read_address2 0x1)
        (register_file$rs1 0x0) (register_file$rs2 0x0) (register_file$store 0x0)
        (register_file$write_address 0x0) (register_file$write_data 0x0)
        (state Load_regs) (vdd 0x1)))
      (Load_mem
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 0) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Addi) (alu$data 0x0) (alu$immediate 0x1) (alu$jump 0x0)
        (alu$jump_target 0x0) (alu$pc 0x4000) (alu$rd 0x1) (alu$rs1 0x0)
        (alu$rs2 0x0) (alu$store 0x1) (decoder$binary_variant Addi)
        (decoder$immediate 0x1) (decoder$instruction_in 0x100013) (decoder$rd 0x0)
        (decoder$rs1 0x0) (decoder$rs2 0x1) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0)
        (dmem_2 0x0) (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x0) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x1)
        (memory_controller$error 0x0) (memory_controller$instruction 0x100013)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x0)
        (memory_controller$program_counter 0x4000)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0x0) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x0)
        (register_file$read_address1 0x0) (register_file$read_address2 0x1)
        (register_file$rs1 0x0) (register_file$rs2 0x0) (register_file$store 0x0)
        (register_file$write_address 0x0) (register_file$write_data 0x0)
        (state Load_mem) (vdd 0x1)))
      (Execute
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 0) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Addi) (alu$data 0x0) (alu$immediate 0x1) (alu$jump 0x0)
        (alu$jump_target 0x0) (alu$pc 0x4000) (alu$rd 0x1) (alu$rs1 0x0)
        (alu$rs2 0x0) (alu$store 0x1) (decoder$binary_variant Addi)
        (decoder$immediate 0x1) (decoder$instruction_in 0x100013) (decoder$rd 0x0)
        (decoder$rs1 0x0) (decoder$rs2 0x1) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0)
        (dmem_2 0x0) (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x0) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x1)
        (memory_controller$error 0x0) (memory_controller$instruction 0x100013)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x0)
        (memory_controller$program_counter 0x4000)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0x0) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x0)
        (register_file$read_address1 0x0) (register_file$read_address2 0x1)
        (register_file$rs1 0x0) (register_file$rs2 0x0) (register_file$store 0x0)
        (register_file$write_address 0x0) (register_file$write_data 0x0)
        (state Execute) (vdd 0x1)))
      (Writeback
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 0) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Addi) (alu$data 0x0) (alu$immediate 0x1) (alu$jump 0x0)
        (alu$jump_target 0x0) (alu$pc 0x4000) (alu$rd 0x1) (alu$rs1 0x0)
        (alu$rs2 0x0) (alu$store 0x1) (decoder$binary_variant Addi)
        (decoder$immediate 0x1) (decoder$instruction_in 0x100013) (decoder$rd 0x0)
        (decoder$rs1 0x0) (decoder$rs2 0x1) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0)
        (dmem_2 0x0) (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x0) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x1)
        (memory_controller$error 0x0) (memory_controller$instruction 0x100013)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x0)
        (memory_controller$program_counter 0x4000)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0x1) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x0)
        (register_file$read_address1 0x0) (register_file$read_address2 0x1)
        (register_file$rs1 0x0) (register_file$rs2 0x0) (register_file$store 0x1)
        (register_file$write_address 0x0) (register_file$write_data 0x1)
        (state Writeback) (vdd 0x1)))
      (Fetch
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 0) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Addi) (alu$data 0x0) (alu$immediate 0x1) (alu$jump 0x0)
        (alu$jump_target 0x0) (alu$pc 0x4004) (alu$rd 0x1) (alu$rs1 0x0)
        (alu$rs2 0x0) (alu$store 0x1) (decoder$binary_variant Addi)
        (decoder$immediate 0x1) (decoder$instruction_in 0x100013) (decoder$rd 0x0)
        (decoder$rs1 0x0) (decoder$rs2 0x1) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0)
        (dmem_2 0x0) (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x0) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x1)
        (memory_controller$error 0x0) (memory_controller$instruction 0x100013)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x1)
        (memory_controller$program_counter 0x4004)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0x1) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x0)
        (register_file$read_address1 0x0) (register_file$read_address2 0x1)
        (register_file$rs1 0x0) (register_file$rs2 0x0) (register_file$store 0x0)
        (register_file$write_address 0x0) (register_file$write_data 0x1)
        (state Fetch) (vdd 0x1)))
      (Decode
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 0) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Addi) (alu$data 0x0) (alu$immediate 0x1) (alu$jump 0x0)
        (alu$jump_target 0x0) (alu$pc 0x4004) (alu$rd 0x1) (alu$rs1 0x0)
        (alu$rs2 0x0) (alu$store 0x1) (decoder$binary_variant Addi)
        (decoder$immediate 0x1) (decoder$instruction_in 0x100513) (decoder$rd 0xa)
        (decoder$rs1 0x0) (decoder$rs2 0x1) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0)
        (dmem_2 0x0) (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x0) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x1)
        (memory_controller$error 0x0) (memory_controller$instruction 0x100513)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x0)
        (memory_controller$program_counter 0x4004)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0x1) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x0)
        (register_file$read_address1 0x0) (register_file$read_address2 0x1)
        (register_file$rs1 0x0) (register_file$rs2 0x0) (register_file$store 0x0)
        (register_file$write_address 0x0) (register_file$write_data 0x1)
        (state Decode) (vdd 0x1)))
      (Load_regs
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 0) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Addi) (alu$data 0x0) (alu$immediate 0x1) (alu$jump 0x0)
        (alu$jump_target 0x0) (alu$pc 0x4004) (alu$rd 0x1) (alu$rs1 0x0)
        (alu$rs2 0x0) (alu$store 0x1) (decoder$binary_variant Addi)
        (decoder$immediate 0x1) (decoder$instruction_in 0x100513) (decoder$rd 0xa)
        (decoder$rs1 0x0) (decoder$rs2 0x1) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0)
        (dmem_2 0x0) (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x1) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x1)
        (memory_controller$error 0x0) (memory_controller$instruction 0x100513)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x0)
        (memory_controller$program_counter 0x4004)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0x1) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x1)
        (register_file$read_address1 0x0) (register_file$read_address2 0x1)
        (register_file$rs1 0x0) (register_file$rs2 0x0) (register_file$store 0x0)
        (register_file$write_address 0xa) (register_file$write_data 0x1)
        (state Load_regs) (vdd 0x1)))
      (Load_mem
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 0) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Addi) (alu$data 0x0) (alu$immediate 0x1) (alu$jump 0x0)
        (alu$jump_target 0x0) (alu$pc 0x4004) (alu$rd 0x1) (alu$rs1 0x0)
        (alu$rs2 0x0) (alu$store 0x1) (decoder$binary_variant Addi)
        (decoder$immediate 0x1) (decoder$instruction_in 0x100513) (decoder$rd 0xa)
        (decoder$rs1 0x0) (decoder$rs2 0x1) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0)
        (dmem_2 0x0) (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x0) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x1)
        (memory_controller$error 0x0) (memory_controller$instruction 0x100513)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x0)
        (memory_controller$program_counter 0x4004)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0x1) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x0)
        (register_file$read_address1 0x0) (register_file$read_address2 0x1)
        (register_file$rs1 0x0) (register_file$rs2 0x0) (register_file$store 0x0)
        (register_file$write_address 0xa) (register_file$write_data 0x1)
        (state Load_mem) (vdd 0x1)))
      (Execute
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 0) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Addi) (alu$data 0x0) (alu$immediate 0x1) (alu$jump 0x0)
        (alu$jump_target 0x0) (alu$pc 0x4004) (alu$rd 0x1) (alu$rs1 0x0)
        (alu$rs2 0x0) (alu$store 0x1) (decoder$binary_variant Addi)
        (decoder$immediate 0x1) (decoder$instruction_in 0x100513) (decoder$rd 0xa)
        (decoder$rs1 0x0) (decoder$rs2 0x1) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0)
        (dmem_2 0x0) (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x0) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x1)
        (memory_controller$error 0x0) (memory_controller$instruction 0x100513)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x0)
        (memory_controller$program_counter 0x4004)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0x1) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x0)
        (register_file$read_address1 0x0) (register_file$read_address2 0x1)
        (register_file$rs1 0x0) (register_file$rs2 0x0) (register_file$store 0x0)
        (register_file$write_address 0xa) (register_file$write_data 0x1)
        (state Execute) (vdd 0x1)))
      (Writeback
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 0) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Addi) (alu$data 0x0) (alu$immediate 0x1) (alu$jump 0x0)
        (alu$jump_target 0x0) (alu$pc 0x4004) (alu$rd 0x1) (alu$rs1 0x0)
        (alu$rs2 0x0) (alu$store 0x1) (decoder$binary_variant Addi)
        (decoder$immediate 0x1) (decoder$instruction_in 0x100513) (decoder$rd 0xa)
        (decoder$rs1 0x0) (decoder$rs2 0x1) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0)
        (dmem_2 0x0) (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x0) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x1)
        (memory_controller$error 0x0) (memory_controller$instruction 0x100513)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x0)
        (memory_controller$program_counter 0x4004)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0x1) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x0)
        (register_file$read_address1 0x0) (register_file$read_address2 0x1)
        (register_file$rs1 0x0) (register_file$rs2 0x0) (register_file$store 0x1)
        (register_file$write_address 0xa) (register_file$write_data 0x1)
        (state Writeback) (vdd 0x1)))
      (Fetch
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 0) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Addi) (alu$data 0x0) (alu$immediate 0x1) (alu$jump 0x0)
        (alu$jump_target 0x0) (alu$pc 0x4008) (alu$rd 0x1) (alu$rs1 0x0)
        (alu$rs2 0x0) (alu$store 0x1) (decoder$binary_variant Addi)
        (decoder$immediate 0x1) (decoder$instruction_in 0x100513) (decoder$rd 0xa)
        (decoder$rs1 0x0) (decoder$rs2 0x1) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0)
        (dmem_2 0x0) (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x0) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x1)
        (memory_controller$error 0x0) (memory_controller$instruction 0x100513)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x1)
        (memory_controller$program_counter 0x4008)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0x1) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x0)
        (register_file$read_address1 0x0) (register_file$read_address2 0x1)
        (register_file$rs1 0x0) (register_file$rs2 0x0) (register_file$store 0x0)
        (register_file$write_address 0xa) (register_file$write_data 0x1)
        (state Fetch) (vdd 0x1)))
      (Decode
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 0) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Addi) (alu$data 0x0) (alu$immediate 0x1) (alu$jump 0x0)
        (alu$jump_target 0x0) (alu$pc 0x4008) (alu$rd 0x1) (alu$rs1 0x0)
        (alu$rs2 0x0) (alu$store 0x1) (decoder$binary_variant Addi)
        (decoder$immediate 0x3) (decoder$instruction_in 0x300593) (decoder$rd 0xb)
        (decoder$rs1 0x0) (decoder$rs2 0x3) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0)
        (dmem_2 0x0) (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x0) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x1)
        (memory_controller$error 0x0) (memory_controller$instruction 0x300593)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x0)
        (memory_controller$program_counter 0x4008)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0x1) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x0)
        (register_file$read_address1 0x0) (register_file$read_address2 0x1)
        (register_file$rs1 0x0) (register_file$rs2 0x0) (register_file$store 0x0)
        (register_file$write_address 0xa) (register_file$write_data 0x1)
        (state Decode) (vdd 0x1)))
      (Load_regs
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 0) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Addi) (alu$data 0x0) (alu$immediate 0x3) (alu$jump 0x0)
        (alu$jump_target 0x0) (alu$pc 0x4008) (alu$rd 0x3) (alu$rs1 0x0)
        (alu$rs2 0x0) (alu$store 0x1) (decoder$binary_variant Addi)
        (decoder$immediate 0x3) (decoder$instruction_in 0x300593) (decoder$rd 0xb)
        (decoder$rs1 0x0) (decoder$rs2 0x3) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0)
        (dmem_2 0x0) (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x1) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x3)
        (memory_controller$error 0x0) (memory_controller$instruction 0x300593)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x0)
        (memory_controller$program_counter 0x4008)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0x1) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x1)
        (register_file$read_address1 0x0) (register_file$read_address2 0x3)
        (register_file$rs1 0x0) (register_file$rs2 0x0) (register_file$store 0x0)
        (register_file$write_address 0xb) (register_file$write_data 0x1)
        (state Load_regs) (vdd 0x1)))
      (Load_mem
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 0) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Addi) (alu$data 0x0) (alu$immediate 0x3) (alu$jump 0x0)
        (alu$jump_target 0x0) (alu$pc 0x4008) (alu$rd 0x3) (alu$rs1 0x0)
        (alu$rs2 0x0) (alu$store 0x1) (decoder$binary_variant Addi)
        (decoder$immediate 0x3) (decoder$instruction_in 0x300593) (decoder$rd 0xb)
        (decoder$rs1 0x0) (decoder$rs2 0x3) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0)
        (dmem_2 0x0) (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x0) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x3)
        (memory_controller$error 0x0) (memory_controller$instruction 0x300593)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x0)
        (memory_controller$program_counter 0x4008)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0x1) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x0)
        (register_file$read_address1 0x0) (register_file$read_address2 0x3)
        (register_file$rs1 0x0) (register_file$rs2 0x0) (register_file$store 0x0)
        (register_file$write_address 0xb) (register_file$write_data 0x1)
        (state Load_mem) (vdd 0x1)))
      (Execute
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 0) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Addi) (alu$data 0x0) (alu$immediate 0x3) (alu$jump 0x0)
        (alu$jump_target 0x0) (alu$pc 0x4008) (alu$rd 0x3) (alu$rs1 0x0)
        (alu$rs2 0x0) (alu$store 0x1) (decoder$binary_variant Addi)
        (decoder$immediate 0x3) (decoder$instruction_in 0x300593) (decoder$rd 0xb)
        (decoder$rs1 0x0) (decoder$rs2 0x3) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0)
        (dmem_2 0x0) (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x0) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x3)
        (memory_controller$error 0x0) (memory_controller$instruction 0x300593)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x0)
        (memory_controller$program_counter 0x4008)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0x1) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x0)
        (register_file$read_address1 0x0) (register_file$read_address2 0x3)
        (register_file$rs1 0x0) (register_file$rs2 0x0) (register_file$store 0x0)
        (register_file$write_address 0xb) (register_file$write_data 0x1)
        (state Execute) (vdd 0x1)))
      (Writeback
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 0) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Addi) (alu$data 0x0) (alu$immediate 0x3) (alu$jump 0x0)
        (alu$jump_target 0x0) (alu$pc 0x4008) (alu$rd 0x3) (alu$rs1 0x0)
        (alu$rs2 0x0) (alu$store 0x1) (decoder$binary_variant Addi)
        (decoder$immediate 0x3) (decoder$instruction_in 0x300593) (decoder$rd 0xb)
        (decoder$rs1 0x0) (decoder$rs2 0x3) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0)
        (dmem_2 0x0) (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x0) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x3)
        (memory_controller$error 0x0) (memory_controller$instruction 0x300593)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x0)
        (memory_controller$program_counter 0x4008)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0x3) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x0)
        (register_file$read_address1 0x0) (register_file$read_address2 0x3)
        (register_file$rs1 0x0) (register_file$rs2 0x0) (register_file$store 0x1)
        (register_file$write_address 0xb) (register_file$write_data 0x3)
        (state Writeback) (vdd 0x1)))
      (Fetch
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 0) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Addi) (alu$data 0x0) (alu$immediate 0x3) (alu$jump 0x0)
        (alu$jump_target 0x0) (alu$pc 0x400c) (alu$rd 0x3) (alu$rs1 0x0)
        (alu$rs2 0x0) (alu$store 0x1) (decoder$binary_variant Addi)
        (decoder$immediate 0x3) (decoder$instruction_in 0x300593) (decoder$rd 0xb)
        (decoder$rs1 0x0) (decoder$rs2 0x3) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0)
        (dmem_2 0x0) (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x0) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x3)
        (memory_controller$error 0x0) (memory_controller$instruction 0x300593)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x1)
        (memory_controller$program_counter 0x400c)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0x3) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x0)
        (register_file$read_address1 0x0) (register_file$read_address2 0x3)
        (register_file$rs1 0x0) (register_file$rs2 0x0) (register_file$store 0x0)
        (register_file$write_address 0xb) (register_file$write_data 0x3)
        (state Fetch) (vdd 0x1)))
      (Decode
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 0) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Addi) (alu$data 0x0) (alu$immediate 0x3) (alu$jump 0x0)
        (alu$jump_target 0x0) (alu$pc 0x400c) (alu$rd 0x3) (alu$rs1 0x0)
        (alu$rs2 0x0) (alu$store 0x1) (decoder$binary_variant Sll)
        (decoder$immediate 0x0) (decoder$instruction_in 0xb51533) (decoder$rd 0xa)
        (decoder$rs1 0xa) (decoder$rs2 0xb) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0)
        (dmem_2 0x0) (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x0) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x3)
        (memory_controller$error 0x0) (memory_controller$instruction 0xb51533)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x0)
        (memory_controller$program_counter 0x400c)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0x3) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x0)
        (register_file$read_address1 0x0) (register_file$read_address2 0x3)
        (register_file$rs1 0x0) (register_file$rs2 0x0) (register_file$store 0x0)
        (register_file$write_address 0xb) (register_file$write_data 0x3)
        (state Decode) (vdd 0x1)))
      (Load_regs
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 0) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Sll) (alu$data 0x0) (alu$immediate 0x0) (alu$jump 0x0)
        (alu$jump_target 0x0) (alu$pc 0x400c) (alu$rd 0x0) (alu$rs1 0x0)
        (alu$rs2 0x0) (alu$store 0x1) (decoder$binary_variant Sll)
        (decoder$immediate 0x0) (decoder$instruction_in 0xb51533) (decoder$rd 0xa)
        (decoder$rs1 0xa) (decoder$rs2 0xb) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0)
        (dmem_2 0x0) (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x1) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x0)
        (memory_controller$error 0x0) (memory_controller$instruction 0xb51533)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x0)
        (memory_controller$program_counter 0x400c)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0x3) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x1)
        (register_file$read_address1 0xa) (register_file$read_address2 0xb)
        (register_file$rs1 0x0) (register_file$rs2 0x0) (register_file$store 0x0)
        (register_file$write_address 0xa) (register_file$write_data 0x3)
        (state Load_regs) (vdd 0x1)))
      (Load_mem
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 0) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Sll) (alu$data 0x0) (alu$immediate 0x0) (alu$jump 0x0)
        (alu$jump_target 0x0) (alu$pc 0x400c) (alu$rd 0x8) (alu$rs1 0x1)
        (alu$rs2 0x3) (alu$store 0x1) (decoder$binary_variant Sll)
        (decoder$immediate 0x0) (decoder$instruction_in 0xb51533) (decoder$rd 0xa)
        (decoder$rs1 0xa) (decoder$rs2 0xb) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0)
        (dmem_2 0x0) (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x0) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x1)
        (memory_controller$error 0x0) (memory_controller$instruction 0xb51533)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x0)
        (memory_controller$program_counter 0x400c)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0x3) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x0)
        (register_file$read_address1 0xa) (register_file$read_address2 0xb)
        (register_file$rs1 0x1) (register_file$rs2 0x3) (register_file$store 0x0)
        (register_file$write_address 0xa) (register_file$write_data 0x3)
        (state Load_mem) (vdd 0x1)))
      (Execute
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 0) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Sll) (alu$data 0x0) (alu$immediate 0x0) (alu$jump 0x0)
        (alu$jump_target 0x0) (alu$pc 0x400c) (alu$rd 0x8) (alu$rs1 0x1)
        (alu$rs2 0x3) (alu$store 0x1) (decoder$binary_variant Sll)
        (decoder$immediate 0x0) (decoder$instruction_in 0xb51533) (decoder$rd 0xa)
        (decoder$rs1 0xa) (decoder$rs2 0xb) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0)
        (dmem_2 0x0) (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x0) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x1)
        (memory_controller$error 0x0) (memory_controller$instruction 0xb51533)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x0)
        (memory_controller$program_counter 0x400c)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0x3) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x0)
        (register_file$read_address1 0xa) (register_file$read_address2 0xb)
        (register_file$rs1 0x1) (register_file$rs2 0x3) (register_file$store 0x0)
        (register_file$write_address 0xa) (register_file$write_data 0x3)
        (state Execute) (vdd 0x1)))
      (Writeback
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 0) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Sll) (alu$data 0x0) (alu$immediate 0x0) (alu$jump 0x0)
        (alu$jump_target 0x0) (alu$pc 0x400c) (alu$rd 0x8) (alu$rs1 0x1)
        (alu$rs2 0x3) (alu$store 0x1) (decoder$binary_variant Sll)
        (decoder$immediate 0x0) (decoder$instruction_in 0xb51533) (decoder$rd 0xa)
        (decoder$rs1 0xa) (decoder$rs2 0xb) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0)
        (dmem_2 0x0) (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x0) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x1)
        (memory_controller$error 0x0) (memory_controller$instruction 0xb51533)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x0)
        (memory_controller$program_counter 0x400c)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0x8) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x0)
        (register_file$read_address1 0xa) (register_file$read_address2 0xb)
        (register_file$rs1 0x1) (register_file$rs2 0x3) (register_file$store 0x1)
        (register_file$write_address 0xa) (register_file$write_data 0x8)
        (state Writeback) (vdd 0x1)))
      (Fetch
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 0) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Sll) (alu$data 0x0) (alu$immediate 0x0) (alu$jump 0x0)
        (alu$jump_target 0x0) (alu$pc 0x4010) (alu$rd 0x8) (alu$rs1 0x1)
        (alu$rs2 0x3) (alu$store 0x1) (decoder$binary_variant Sll)
        (decoder$immediate 0x0) (decoder$instruction_in 0xb51533) (decoder$rd 0xa)
        (decoder$rs1 0xa) (decoder$rs2 0xb) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0)
        (dmem_2 0x0) (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x0) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x1)
        (memory_controller$error 0x0) (memory_controller$instruction 0xb51533)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x1)
        (memory_controller$program_counter 0x4010)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0x8) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x0)
        (register_file$read_address1 0xa) (register_file$read_address2 0xb)
        (register_file$rs1 0x1) (register_file$rs2 0x3) (register_file$store 0x0)
        (register_file$write_address 0xa) (register_file$write_data 0x8)
        (state Fetch) (vdd 0x1)))
      (Decode
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 0) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Sll) (alu$data 0x0) (alu$immediate 0x0) (alu$jump 0x0)
        (alu$jump_target 0x0) (alu$pc 0x4010) (alu$rd 0x8) (alu$rs1 0x1)
        (alu$rs2 0x3) (alu$store 0x1) (decoder$binary_variant Add)
        (decoder$immediate 0x0) (decoder$instruction_in 0xb50533) (decoder$rd 0xa)
        (decoder$rs1 0xa) (decoder$rs2 0xb) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0)
        (dmem_2 0x0) (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x0) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x1)
        (memory_controller$error 0x0) (memory_controller$instruction 0xb50533)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x0)
        (memory_controller$program_counter 0x4010)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0x8) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x0)
        (register_file$read_address1 0xa) (register_file$read_address2 0xb)
        (register_file$rs1 0x1) (register_file$rs2 0x3) (register_file$store 0x0)
        (register_file$write_address 0xa) (register_file$write_data 0x8)
        (state Decode) (vdd 0x1)))
      (Load_regs
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 0) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Add) (alu$data 0x0) (alu$immediate 0x0) (alu$jump 0x0)
        (alu$jump_target 0x0) (alu$pc 0x4010) (alu$rd 0x4) (alu$rs1 0x1)
        (alu$rs2 0x3) (alu$store 0x1) (decoder$binary_variant Add)
        (decoder$immediate 0x0) (decoder$instruction_in 0xb50533) (decoder$rd 0xa)
        (decoder$rs1 0xa) (decoder$rs2 0xb) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0)
        (dmem_2 0x0) (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x1) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x1)
        (memory_controller$error 0x0) (memory_controller$instruction 0xb50533)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x0)
        (memory_controller$program_counter 0x4010)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0x8) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x1)
        (register_file$read_address1 0xa) (register_file$read_address2 0xb)
        (register_file$rs1 0x1) (register_file$rs2 0x3) (register_file$store 0x0)
        (register_file$write_address 0xa) (register_file$write_data 0x8)
        (state Load_regs) (vdd 0x1)))
      (Load_mem
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 0) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Add) (alu$data 0x0) (alu$immediate 0x0) (alu$jump 0x0)
        (alu$jump_target 0x0) (alu$pc 0x4010) (alu$rd 0xb) (alu$rs1 0x8)
        (alu$rs2 0x3) (alu$store 0x1) (decoder$binary_variant Add)
        (decoder$immediate 0x0) (decoder$instruction_in 0xb50533) (decoder$rd 0xa)
        (decoder$rs1 0xa) (decoder$rs2 0xb) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0)
        (dmem_2 0x0) (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x0) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x8)
        (memory_controller$error 0x0) (memory_controller$instruction 0xb50533)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x0)
        (memory_controller$program_counter 0x4010)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0x8) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x0)
        (register_file$read_address1 0xa) (register_file$read_address2 0xb)
        (register_file$rs1 0x8) (register_file$rs2 0x3) (register_file$store 0x0)
        (register_file$write_address 0xa) (register_file$write_data 0x8)
        (state Load_mem) (vdd 0x1)))
      (Execute
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 0) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Add) (alu$data 0x0) (alu$immediate 0x0) (alu$jump 0x0)
        (alu$jump_target 0x0) (alu$pc 0x4010) (alu$rd 0xb) (alu$rs1 0x8)
        (alu$rs2 0x3) (alu$store 0x1) (decoder$binary_variant Add)
        (decoder$immediate 0x0) (decoder$instruction_in 0xb50533) (decoder$rd 0xa)
        (decoder$rs1 0xa) (decoder$rs2 0xb) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0)
        (dmem_2 0x0) (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x0) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x8)
        (memory_controller$error 0x0) (memory_controller$instruction 0xb50533)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x0)
        (memory_controller$program_counter 0x4010)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0x8) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x0)
        (register_file$read_address1 0xa) (register_file$read_address2 0xb)
        (register_file$rs1 0x8) (register_file$rs2 0x3) (register_file$store 0x0)
        (register_file$write_address 0xa) (register_file$write_data 0x8)
        (state Execute) (vdd 0x1)))
      (Writeback
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 0) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Add) (alu$data 0x0) (alu$immediate 0x0) (alu$jump 0x0)
        (alu$jump_target 0x0) (alu$pc 0x4010) (alu$rd 0xb) (alu$rs1 0x8)
        (alu$rs2 0x3) (alu$store 0x1) (decoder$binary_variant Add)
        (decoder$immediate 0x0) (decoder$instruction_in 0xb50533) (decoder$rd 0xa)
        (decoder$rs1 0xa) (decoder$rs2 0xb) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0)
        (dmem_2 0x0) (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x0) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x8)
        (memory_controller$error 0x0) (memory_controller$instruction 0xb50533)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x0)
        (memory_controller$program_counter 0x4010)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0xb) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x0)
        (register_file$read_address1 0xa) (register_file$read_address2 0xb)
        (register_file$rs1 0x8) (register_file$rs2 0x3) (register_file$store 0x1)
        (register_file$write_address 0xa) (register_file$write_data 0xb)
        (state Writeback) (vdd 0x1)))
      (Fetch
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 0) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Add) (alu$data 0x0) (alu$immediate 0x0) (alu$jump 0x0)
        (alu$jump_target 0x0) (alu$pc 0x4014) (alu$rd 0xb) (alu$rs1 0x8)
        (alu$rs2 0x3) (alu$store 0x1) (decoder$binary_variant Add)
        (decoder$immediate 0x0) (decoder$instruction_in 0xb50533) (decoder$rd 0xa)
        (decoder$rs1 0xa) (decoder$rs2 0xb) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0)
        (dmem_2 0x0) (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x0) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x8)
        (memory_controller$error 0x0) (memory_controller$instruction 0xb50533)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x1)
        (memory_controller$program_counter 0x4014)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0xb) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x0)
        (register_file$read_address1 0xa) (register_file$read_address2 0xb)
        (register_file$rs1 0x8) (register_file$rs2 0x3) (register_file$store 0x0)
        (register_file$write_address 0xa) (register_file$write_data 0xb)
        (state Fetch) (vdd 0x1)))
      (Decode
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 0) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Add) (alu$data 0x0) (alu$immediate 0x0) (alu$jump 0x0)
        (alu$jump_target 0x0) (alu$pc 0x4014) (alu$rd 0xb) (alu$rs1 0x8)
        (alu$rs2 0x3) (alu$store 0x1) (decoder$binary_variant Invalid)
        (decoder$immediate 0x0) (decoder$instruction_in 0x0) (decoder$rd 0x0)
        (decoder$rs1 0x0) (decoder$rs2 0x0) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0)
        (dmem_2 0x0) (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x0) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x8)
        (memory_controller$error 0x0) (memory_controller$instruction 0x0)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x0)
        (memory_controller$program_counter 0x4014)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0xb) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x0)
        (register_file$read_address1 0xa) (register_file$read_address2 0xb)
        (register_file$rs1 0x8) (register_file$rs2 0x3) (register_file$store 0x0)
        (register_file$write_address 0xa) (register_file$write_data 0xb)
        (state Decode) (vdd 0x1)))
      (Load_regs
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 1) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Invalid) (alu$data 0x0) (alu$immediate 0x0)
        (alu$jump 0x0) (alu$jump_target 0x0) (alu$pc 0x4014) (alu$rd 0x0)
        (alu$rs1 0x8) (alu$rs2 0x3) (alu$store 0x0)
        (decoder$binary_variant Invalid) (decoder$immediate 0x0)
        (decoder$instruction_in 0x0) (decoder$rd 0x0) (decoder$rs1 0x0)
        (decoder$rs2 0x0) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0) (dmem_2 0x0)
        (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x1) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x8)
        (memory_controller$error 0x0) (memory_controller$instruction 0x0)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x0)
        (memory_controller$program_counter 0x4014)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0xb) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x1)
        (register_file$read_address1 0x0) (register_file$read_address2 0x0)
        (register_file$rs1 0x8) (register_file$rs2 0x3) (register_file$store 0x0)
        (register_file$write_address 0x0) (register_file$write_data 0xb)
        (state Load_regs) (vdd 0x1)))
      (Error
       ((clock 0) (clear 0) (uart ((write_done 0) (read_data 0) (read_done 0))))
       ((error 1) (uart ((write_data 65) (write_ready 1) (read_ready 1))))
       ((alu$binary_variant Invalid) (alu$data 0x0) (alu$immediate 0x0)
        (alu$jump 0x0) (alu$jump_target 0x0) (alu$pc 0x4014) (alu$rd 0x0)
        (alu$rs1 0x0) (alu$rs2 0x0) (alu$store 0x0)
        (decoder$binary_variant Invalid) (decoder$immediate 0x0)
        (decoder$instruction_in 0x0) (decoder$rd 0x0) (decoder$rs1 0x0)
        (decoder$rs2 0x0) (dmem 0x0) (dmem_0 0x0) (dmem_1 0x0) (dmem_2 0x0)
        (gnd 0x0) (imem 0x0) (imem_0 0x0) (imem_1 0x0) (imem_2 0x0)
        (load_registers 0x0) (memory_controller$binary_variant Byte)
        (memory_controller$clock 0x0) (memory_controller$data_address 0x0)
        (memory_controller$error 0x0) (memory_controller$instruction 0x0)
        (memory_controller$load 0x0) (memory_controller$load_instruction 0x0)
        (memory_controller$program_counter 0x4014)
        (memory_controller$read_data 0x0) (memory_controller$store 0x0)
        (memory_controller$write_data 0xb) (register_file 0x0)
        (register_file$clock 0x0) (register_file$load 0x0)
        (register_file$read_address1 0x0) (register_file$read_address2 0x0)
        (register_file$rs1 0x0) (register_file$rs2 0x0) (register_file$store 0x0)
        (register_file$write_address 0x0) (register_file$write_data 0xb)
        (state Error) (vdd 0x1))) |}]
  ;;
end
