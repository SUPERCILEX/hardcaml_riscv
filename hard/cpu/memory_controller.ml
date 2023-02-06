open! Core
open Hardcaml

(* TODO add boot ROM *)
(* TODO handle single byte memory ops correctly (split memory into byte chunks) *)

module I = struct
  type 'a t =
    { clock : 'a
    ; load_instruction : 'a
    ; load : 'a
    ; store : 'a
    ; program_counter : 'a [@bits Parameters.word_size]
    ; data_address : 'a [@bits Parameters.word_size]
    ; data : 'a [@bits Parameters.word_size] [@rtlname "data_in"]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { instruction : 'a [@bits 32]
    ; data : 'a [@bits Parameters.word_size] [@rtlname "data_out"]
    ; error : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module Route = struct
  type t =
    { address : Signal.t
    ; error : Signal.t
    }
end

let ram
  ~clock
  ~write_address
  ~read_address1
  ~read_address2
  ~write_enable
  ~read_enable1
  ~read_enable2
  ~write_data
  =
  match
    Ram.create
      ~name:"memory"
      ~collision_mode:Read_before_write
      ~size:(Parameters.imem_size + Parameters.dmem_size)
      ~write_ports:
        [| { Ram.Write_port.write_clock = clock; write_address; write_enable; write_data }
        |]
      ~read_ports:
        [| { Ram.Read_port.read_clock = clock
           ; read_address = read_address1
           ; read_enable = read_enable1
           }
         ; { Ram.Read_port.read_clock = clock
           ; read_address = read_address2
           ; read_enable = read_enable2
           }
        |]
      ()
  with
  | [| a; b |] -> a, b
  | _ -> assert false
;;

let router ~address =
  let open Signal in
  let routed_address = Always.Variable.wire ~default:(zero (width address)) in
  let error = Always.Variable.wire ~default:gnd in
  Always.(
    compile
      Parameters.
        [ if_
            (address >:. stack_top - dmem_size &: (address <:. stack_top))
            [ routed_address <-- negate address +:. stack_top ]
          @@ elif
               (address >:. code_bottom &: (address <:. code_bottom + imem_size))
               [ routed_address <-- address -:. code_bottom ]
          @@ [ error <-- vdd ]
        ]);
  { Route.address = routed_address.value; error = error.value }
;;

let create (scope : Scope.t) (i : _ I.t) =
  let open Signal in
  let routed_pc = wire Parameters.word_size in
  let routed_data_address = wire Parameters.word_size in
  let raw_instruction, data_out =
    ram
      ~clock:i.clock
      ~write_address:routed_data_address
      ~read_address1:routed_pc
      ~read_address2:routed_data_address
      ~write_enable:i.store
      ~read_enable1:i.load_instruction
      ~read_enable2:i.load
      ~write_data:(Alu.flip_endianness i.data)
  in
  let { Route.address = pc; error = pc_error } = router ~address:i.program_counter in
  routed_pc <== pc;
  let { Route.address = data_address; error = data_error } =
    router ~address:i.data_address
  in
  routed_data_address <== data_address;
  let _debugging =
    let ( -- ) = Scope.naming scope in
    ignore (routed_pc -- "routed_pc");
    ignore (routed_data_address -- "routed_data_address");
    ignore (pc -- "pc");
    ignore (data_address -- "data_address");
    ignore (pc_error -- "pc_error");
    ignore (data_error -- "data_error");
    ignore (raw_instruction -- "raw_instruction");
    ignore (data_out -- "data_out")
  in
  { O.instruction = Alu.flip_endianness raw_instruction
  ; data = Alu.flip_endianness data_out
  ; error = pc_error &: i.load_instruction |: (data_error &: (i.load |: i.store))
  }
;;

let circuit scope =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"memory_controller" create
;;

module Tests = struct
  module Simulator = Cyclesim.With_interface (I) (O)
  module Waveform = Hardcaml_waveterm.Waveform

  let test_bench (sim : (_ I.t, _ O.t) Cyclesim.t) =
    let inputs, outputs = Cyclesim.inputs sim, Cyclesim.outputs sim in
    let step () =
      Cyclesim.cycle sim;
      Stdio.print_s
        ([%sexp_of: Bits.t I.t * Bits.t O.t]
           (I.map inputs ~f:(fun p -> !p), O.map outputs ~f:(fun p -> !p)));
      Stdio.print_endline ""
    in
    Cyclesim.reset sim;
    inputs.data := Bits.of_int ~width:Parameters.word_size 0xdeadbeef;
    inputs.data_address
      := Bits.of_int
           ~width:Parameters.word_size
           (Parameters.stack_top - Parameters.word_size);
    inputs.program_counter
      := Bits.of_int ~width:Parameters.word_size Parameters.code_bottom;
    inputs.store := Bits.gnd;
    inputs.load := Bits.gnd;
    inputs.load_instruction := Bits.gnd;
    step ();
    inputs.store := Bits.vdd;
    step ();
    inputs.store := Bits.gnd;
    step ();
    inputs.load := Bits.vdd;
    step ();
    inputs.load := Bits.gnd;
    inputs.data_address := !(inputs.program_counter);
    inputs.store := Bits.vdd;
    step ();
    inputs.store := Bits.gnd;
    inputs.load_instruction := Bits.vdd;
    step ()
  ;;

  let sim () =
    let scope = Scope.create ~flatten_design:true () in
    let sim = Simulator.create ~config:Cyclesim.Config.trace_all (create scope) in
    test_bench sim
  ;;

  let%expect_test "RV32I" =
    sim ();
    [%expect
      {|
      (((clock 0) (load_instruction 0) (load 0) (store 0)
        (program_counter 00000000000000000100000000000000)
        (data_address 01111111111111111111111111100000)
        (data 11011110101011011011111011101111))
       ((instruction 00000000000000000000000000000000)
        (data 00000000000000000000000000000000) (error 0)))

      (((clock 0) (load_instruction 0) (load 0) (store 1)
        (program_counter 00000000000000000100000000000000)
        (data_address 01111111111111111111111111100000)
        (data 11011110101011011011111011101111))
       ((instruction 00000000000000000000000000000000)
        (data 00000000000000000000000000000000) (error 0)))

      (((clock 0) (load_instruction 0) (load 0) (store 0)
        (program_counter 00000000000000000100000000000000)
        (data_address 01111111111111111111111111100000)
        (data 11011110101011011011111011101111))
       ((instruction 00000000000000000000000000000000)
        (data 00000000000000000000000000000000) (error 0)))

      (((clock 0) (load_instruction 0) (load 1) (store 0)
        (program_counter 00000000000000000100000000000000)
        (data_address 01111111111111111111111111100000)
        (data 11011110101011011011111011101111))
       ((instruction 00000000000000000000000000000000)
        (data 11011110101011011011111011101111) (error 0)))

      (((clock 0) (load_instruction 0) (load 0) (store 1)
        (program_counter 00000000000000000100000000000000)
        (data_address 00000000000000000100000000000000)
        (data 11011110101011011011111011101111))
       ((instruction 00000000000000000000000000000000)
        (data 11011110101011011011111011101111) (error 1)))

      (((clock 0) (load_instruction 1) (load 0) (store 0)
        (program_counter 00000000000000000100000000000000)
        (data_address 00000000000000000100000000000000)
        (data 11011110101011011011111011101111))
       ((instruction 11011110101011011011111011101111)
        (data 11011110101011011011111011101111) (error 1))) |}]
  ;;
end
