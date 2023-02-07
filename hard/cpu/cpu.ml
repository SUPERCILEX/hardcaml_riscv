open! Core
open Hardcaml

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { _unused : 'a } [@@deriving sexp_of, hardcaml]
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

let register_file
  ~clock
  ~write_address
  ~read_address1
  ~read_address2
  ~write_enable
  ~read_enable
  ~write_data
  =
  let open Signal in
  let _checks =
    assert (width write_address = 5);
    assert (width read_address1 = 5);
    assert (width read_address2 = 5);
    assert (width write_data = Parameters.word_size)
  in
  match
    Ram.create
      ~name:"register_file"
      ~collision_mode:Read_before_write
      ~size:32
      ~write_ports:
        [| { Ram.Write_port.write_clock = clock; write_address; write_enable; write_data }
        |]
      ~read_ports:
        [| { Ram.Read_port.read_clock = clock; read_address = read_address1; read_enable }
         ; { Ram.Read_port.read_clock = clock; read_address = read_address2; read_enable }
        |]
      ()
  with
  | [| rs1; rs2 |] -> rs1, rs2
  | _ -> assert false
;;

let do_on_load instruction ~s =
  Instruction.Binary.Of_always.match_
    ~default:[]
    instruction
    (Instruction.RV32I.[ Lb; Lh; Lw ] |> List.map ~f:(fun i -> i, s))
;;

let do_on_store instruction ~s =
  Instruction.Binary.Of_always.match_
    ~default:[]
    instruction
    (Instruction.RV32I.[ Sb; Sh; Sw ] |> List.map ~f:(fun i -> i, s))
;;

let create (scope : Scope.t) (i : _ I.t) =
  let open Signal in
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let program_counter =
    Always.Variable.reg
      ~width:Parameters.word_size
      (Reg_spec.override
         spec
         ~clear_to:(of_int ~width:Parameters.word_size Parameters.code_bottom))
  in
  let load_instruction = Always.Variable.wire ~default:gnd in
  let load = Always.Variable.wire ~default:gnd in
  let store = Always.Variable.wire ~default:gnd in
  let data_address = Always.Variable.wire ~default:(zero Parameters.word_size) in
  let data_size = Memory_controller.Size.Binary.Of_always.wire zero in
  let data_out = Always.Variable.wire ~default:(zero Parameters.word_size) in
  let { Memory_controller.O.instruction = raw_instruction
      ; data = data_in
      ; error = invalid_address
      }
    =
    Memory_controller.circuit
      scope
      { Memory_controller.I.clock = i.clock
      ; load_instruction = load_instruction.value
      ; load = load.value
      ; store = store.value
      ; program_counter = program_counter.value
      ; data_address = data_address.value
      ; data_size = Memory_controller.Size.Binary.Of_always.value data_size
      ; data = data_out.value
      }
  in
  let decoder = Decoder.circuit scope { Decoder.I.instruction = raw_instruction } in
  let decoder_regs = decoder |> Decoder.O.map ~f:(fun s -> reg spec s) in
  let alu_store = wire 1 in
  let alu_rd = wire Parameters.word_size in
  let sm = Always.State_machine.create (module State) spec in
  let rs1, rs2 =
    register_file
      ~clock:i.clock
      ~write_address:decoder.rd
      ~read_address1:decoder.rs1
      ~read_address2:decoder.rs2
      ~write_enable:alu_store
      ~read_enable:(sm.is Decode_and_load)
      ~write_data:alu_rd
  in
  let alu =
    Alu.circuit
      scope
      { Alu.I.pc = program_counter.value
      ; data = data_in
      ; instruction = decoder_regs.instruction
      ; rs1
      ; rs2
      ; immediate = decoder_regs.immediate
      }
    |> Alu.O.map ~f:(fun s -> reg spec s)
  in
  alu_store <== alu.store;
  alu_rd <== alu.rd;
  let _debugging =
    let ( -- ) = Scope.naming scope in
    ignore (sm.current -- "state")
  in
  Always.(
    compile
      [ sm.switch
          [ Fetch, [ load_instruction <-- vdd; sm.set_next Decode_and_load ]
          ; ( Decode_and_load
            , [ do_on_load
                  decoder.instruction
                  ~s:[ load <-- vdd; data_address <-- rs1 +: decoder.immediate ]
              ; Instruction.Binary.Of_always.match_
                  ~default:[]
                  decoder.instruction
                  ([ [ Instruction.RV32I.Lb; Lbu ], Memory_controller.Size.Enum.Byte
                   ; [ Lh; Lhu ], Half_word
                   ; [ Lw ], Word
                   ]
                  |> List.map ~f:(fun (instructions, s) ->
                       List.map instructions ~f:(fun i -> i, s))
                  |> List.concat
                  |> List.map ~f:(fun (i, s) ->
                       ( i
                       , [ Memory_controller.Size.Binary.(
                             Of_always.assign data_size (Of_signal.of_enum s))
                         ] )))
              ; if_
                  (Instruction.Binary.Of_signal.is decoder.instruction Invalid)
                  [ sm.set_next Error ]
                  [ sm.set_next Execute ]
              ] )
          ; Execute, [ sm.set_next Writeback ]
          ; ( Writeback
            , [ if_
                  alu.jump
                  [ program_counter <-- alu.jump_target ]
                  [ program_counter <-- program_counter.value +:. 4 ]
              ; do_on_store
                  decoder_regs.instruction
                  ~s:
                    [ store <-- vdd
                    ; data_address <-- rs1 +: decoder_regs.immediate
                    ; data_out <-- alu.rd
                    ]
              ; Instruction.Binary.Of_always.match_
                  ~default:[]
                  decoder.instruction
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
              ] )
          ; Error, []
          ]
      ; when_ invalid_address [ sm.set_next Error ]
      ]);
  { O._unused = load_instruction.value }
;;

let circuit scope =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"cpu_control" create
;;

let root scope =
  Circuit.create_with_interface (module I) (module O) ~name:"cpu" (circuit scope)
;;

module Tests = struct
  open Core
  module Simulator = Cyclesim.With_interface (I) (O)
  module Waveform = Hardcaml_waveterm.Waveform

  let test_bench (sim : (_ I.t, _ O.t) Cyclesim.t) =
    let open Bits in
    let inputs, outputs = Cyclesim.inputs sim, Cyclesim.outputs sim in
    let print_state_and_outputs () =
      let state =
        List.nth_exn
          State.all
          (to_int
             !(List.Assoc.find_exn
                 (Cyclesim.internal_ports sim)
                 "state"
                 ~equal:String.equal))
      in
      Stdio.print_s
        ([%sexp_of: State.t * int O.t] (state, O.map outputs ~f:(fun p -> to_int !p)))
    in
    let reset () =
      Cyclesim.reset sim;
      inputs.clear := vdd;
      Cyclesim.cycle sim;
      inputs.clear := gnd;
      print_state_and_outputs ()
    in
    let run () =
      for _ = 0 to 11 do
        Cyclesim.cycle sim;
        print_state_and_outputs ()
      done
    in
    reset ();
    run ()
  ;;

  let sim () =
    let scope = Scope.create ~flatten_design:true () in
    let sim = Simulator.create ~config:Cyclesim.Config.trace_all (create scope) in
    test_bench sim
  ;;

  let waves () =
    let scope = Scope.create ~flatten_design:true () in
    let sim = Simulator.create ~config:Cyclesim.Config.trace_all (create scope) in
    let waves, sim = Waveform.create sim in
    test_bench sim;
    let () =
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
                   (List.map State.all ~f:(fun t -> State.sexp_of_t t |> Sexp.to_string)))
          ]
      in
      Waveform.print
        waves
        ~display_height:25
        ~display_width:150
        ~display_rules:(input_rules @ output_rules @ [ default ])
    in
    waves
  ;;

  let%expect_test "Simple" =
    sim ();
    [%expect
      {|
      (Fetch ((_unused 1)))
      (Fetch ((_unused 0)))
      (Decode_and_load ((_unused 0)))
      (Error ((_unused 0)))
      (Error ((_unused 0)))
      (Error ((_unused 0)))
      (Error ((_unused 0)))
      (Error ((_unused 0)))
      (Error ((_unused 0)))
      (Error ((_unused 0)))
      (Error ((_unused 0)))
      (Error ((_unused 0)))
      (Error ((_unused 0))) |}]
  ;;
end
