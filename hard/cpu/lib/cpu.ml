open! Core
open Hardcaml
module Uart = Uart

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
    (Instruction.RV32I.[ Lb; Lbu; Lh; Lhu; Lw ] |> List.map ~f:(fun i -> i, s))
;;

let do_on_store instruction ~s =
  Instruction.Binary.Of_always.match_
    ~default:[]
    instruction
    (Instruction.RV32I.[ Sb; Sh; Sw ] |> List.map ~f:(fun i -> i, s))
;;

let fetch
  ~(sm : State.t Always.State_machine.t)
  ~memory_controller:({ load_instruction; _ } : _ Memory_controller.I.t)
  =
  let open Signal in
  Always.[ load_instruction <-- vdd; sm.set_next Decode ]
;;

let decode ~(sm : State.t Always.State_machine.t) = [ sm.set_next Load_regs ]

let load_regs
  ~(sm : State.t Always.State_machine.t)
  ~decoded:({ instruction; _ } : _ Decoder.O.t)
  ~load_registers
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

let load_mem
  ~(sm : State.t Always.State_machine.t)
  ~loaded:({ instruction; rs1; rs2 = _; immediate; _ } : _ Alu.I.t)
  ~memory_controller:
    ({ clock = _
     ; load_instruction = _
     ; load
     ; store = _
     ; program_counter = _
     ; data_address
     ; data_size
     ; data = _
     } :
      _ Memory_controller.I.t)
  =
  let open Signal in
  Always.
    [ do_on_load instruction ~s:[ load <-- vdd; data_address <-- rs1 +: immediate ]
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

let writeback
  ~(sm : State.t Always.State_machine.t)
  ~loaded:({ instruction; rs1; immediate; pc = _; data = _; rs2 = _ } : _ Alu.I.t)
  ~alu:({ rd; store = _; jump; jump_target } : _ Alu.O.t)
  ~memory_controller:
    ({ clock = _
     ; load_instruction = _
     ; load = _
     ; store = mem_store
     ; program_counter
     ; data_address
     ; data_size
     ; data
     } :
      _ Memory_controller.I.t)
  =
  let open Signal in
  Always.
    [ if_
        jump
        [ program_counter <-- jump_target ]
        [ program_counter <-- program_counter.value +:. 4 ]
    ; do_on_store
        instruction
        ~s:[ mem_store <-- vdd; data_address <-- rs1 +: immediate; data <-- rd ]
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

let create (scope : Scope.t) ({ clock; clear; uart } : _ I.t) =
  let open Signal in
  let spec = Reg_spec.create ~clock ~clear () in
  let sm = Always.State_machine.create (module State) spec in
  let _debugging =
    let ( -- ) = Scope.naming scope in
    ignore (sm.current -- "state")
  in
  let memory_controller =
    { (Memory_controller.I.Of_always.wire zero) with
      program_counter =
        Always.Variable.reg
          ~width:Parameters.word_size
          (Reg_spec.override
             spec
             ~clear_to:(of_int ~width:Parameters.word_size Parameters.code_bottom))
    }
  in
  let { Memory_controller.O.instruction = raw_instruction
      ; data = data_in
      ; error = invalid_address
      }
    =
    Memory_controller.circuit
      scope
      { (Memory_controller.I.map memory_controller ~f:Always.Variable.value) with clock }
  in
  let decoder =
    Decoder.circuit scope { Decoder.I.instruction = raw_instruction }
    |> Decoder.O.map ~f:(reg spec)
  in
  let alu_feedback = Alu.O.Of_signal.wires () in
  let load_registers = Always.Variable.wire ~default:gnd in
  let rs1, rs2 =
    let { Alu.O.store = reg_store; rd = alu_result; _ } = alu_feedback in
    let { Decoder.O.rd; rs1; rs2; _ } = decoder in
    register_file
      ~clock
      ~write_address:rd
      ~read_address1:rs1
      ~read_address2:rs2
      ~write_enable:reg_store
      ~read_enable:load_registers.value
      ~write_data:alu_result
  in
  let loaded =
    let { Decoder.O.instruction; immediate; _ } = decoder in
    { Alu.I.pc = memory_controller.program_counter.value
    ; data = data_in
    ; instruction
    ; rs1
    ; rs2
    ; immediate
    }
  in
  let alu = Alu.circuit scope loaded |> Alu.O.map ~f:(reg spec) in
  Alu.O.iter2 alu_feedback alu ~f:( <== );
  Always.(
    compile
      [ sm.switch
          [ Fetch, fetch ~sm ~memory_controller
          ; Decode, decode ~sm
          ; Load_regs, load_regs ~sm ~decoded:decoder ~load_registers
          ; Load_mem, load_mem ~sm ~loaded ~memory_controller
          ; Execute, execute ~sm
          ; Writeback, writeback ~sm ~loaded ~alu ~memory_controller
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
  H.hierarchical ~scope ~name:"cpu" create
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
      inputs.clear := gnd
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
      (Fetch ((error 0) (uart ((write_data 65) (write_ready 1) (read_ready 1)))))
      (Decode ((error 0) (uart ((write_data 65) (write_ready 1) (read_ready 1)))))
      (Load_regs
       ((error 1) (uart ((write_data 65) (write_ready 1) (read_ready 1)))))
      (Error ((error 1) (uart ((write_data 65) (write_ready 1) (read_ready 1)))))
      (Error ((error 1) (uart ((write_data 65) (write_ready 1) (read_ready 1)))))
      (Error ((error 1) (uart ((write_data 65) (write_ready 1) (read_ready 1)))))
      (Error ((error 1) (uart ((write_data 65) (write_ready 1) (read_ready 1)))))
      (Error ((error 1) (uart ((write_data 65) (write_ready 1) (read_ready 1)))))
      (Error ((error 1) (uart ((write_data 65) (write_ready 1) (read_ready 1)))))
      (Error ((error 1) (uart ((write_data 65) (write_ready 1) (read_ready 1)))))
      (Error ((error 1) (uart ((write_data 65) (write_ready 1) (read_ready 1)))))
      (Error ((error 1) (uart ((write_data 65) (write_ready 1) (read_ready 1))))) |}]
  ;;
end
