open! Core
open Hardcaml

module Op = struct
  module RV32I = struct
    type t =
      | Add
      | Sub
      | And
      | Or
      | Xor
      | Shift_left_logical
      | Shift_right_logical
      | Shift_right_arithmetic
    [@@deriving sexp_of, compare, enumerate]
  end

  include Hardcaml.Interface.Make_enums (RV32I)
end

module I = struct
  type 'a t =
    { op : 'a Op.Binary.t
    ; a : 'a [@bits Parameters.word_size]
    ; b : 'a [@bits Parameters.word_size]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { result : 'a [@bits Parameters.word_size] } [@@deriving sexp_of, hardcaml]
end

let create (_scope : Scope.t) (i : _ I.t) =
  let open Signal in
  let shift_mux ~f =
    mux (sel_bottom i.b 5) (List.init 32 ~f:(fun shift -> f i.a shift))
  in
  { O.result =
      Op.Binary.Of_signal.match_
        i.op
        [ Add, i.a +: i.b
        ; Sub, i.a -: i.b
        ; And, i.a &: i.b
        ; Or, i.a |: i.b
        ; Xor, i.a ^: i.b
        ; Shift_left_logical, shift_mux ~f:sll
        ; Shift_right_logical, shift_mux ~f:srl
        ; Shift_right_arithmetic, shift_mux ~f:sra
        ]
  }
;;

let circuit scope =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"alu" create
;;

module Tests = struct
  open Core
  module Simulator = Cyclesim.With_interface (I) (O)
  module Waveform = Hardcaml_waveterm.Waveform

  let test_bench (sim : (_ I.t, _ O.t) Cyclesim.t) =
    let inputs, outputs = Cyclesim.inputs sim, Cyclesim.outputs sim in
    let print_state_ints () =
      let op = Op.Binary.sim_get_exn inputs.op in
      let a = Bits.to_int !(inputs.a) in
      let b = Bits.to_int !(inputs.b) in
      let result = Bits.to_int !(outputs.result) in
      Stdio.print_s [%message (op : Op.RV32I.t) (a : int) (b : int) (result : int)]
    in
    let print_state_bits () =
      let op = Op.Binary.sim_get_exn inputs.op in
      let a = !(inputs.a) in
      let b = !(inputs.b) in
      let result = !(outputs.result) in
      Stdio.print_s
        [%message (op : Op.RV32I.t) (a : Bits.t) (b : Bits.t) (result : Bits.t)]
    in
    let run op a b =
      Op.Binary.sim_set inputs.op op;
      inputs.a := a;
      inputs.b := b;
      Cyclesim.cycle sim;
      match op with
      | Add | Sub -> print_state_ints ()
      | And | Or | Xor | Shift_left_logical | Shift_right_logical | Shift_right_arithmetic
        -> print_state_bits ()
    in
    let bit_num = Bits.of_int ~width:Parameters.word_size in
    run Add (bit_num 69) (bit_num 42);
    run Sub (bit_num 69) (bit_num 42);
    run And (bit_num 69) (bit_num 42);
    run Or (bit_num 69) (bit_num 42);
    run Xor (bit_num 69) (bit_num 438);
    run Shift_left_logical (bit_num 2147483776) (bit_num 5);
    run Shift_right_logical (bit_num 2147483776) (bit_num 5);
    run Shift_right_arithmetic (bit_num 2147483776) (bit_num 5)
  ;;

  let sim () =
    let scope = Scope.create ~flatten_design:true () in
    let sim = Simulator.create ~config:Cyclesim.Config.trace_all (create scope) in
    test_bench sim
  ;;

  let%expect_test "Simple" =
    sim ();
    [%expect
      {|
      ((op Add) (a 69) (b 42) (result 111))
      ((op Sub) (a 69) (b 42) (result 27))
      ((op And) (a 00000000000000000000000001000101)
       (b 00000000000000000000000000101010)
       (result 00000000000000000000000000000000))
      ((op Or) (a 00000000000000000000000001000101)
       (b 00000000000000000000000000101010)
       (result 00000000000000000000000001101111))
      ((op Xor) (a 00000000000000000000000001000101)
       (b 00000000000000000000000110110110)
       (result 00000000000000000000000111110011))
      ((op Shift_left_logical) (a 10000000000000000000000010000000)
       (b 00000000000000000000000000000101)
       (result 00000000000000000001000000000000))
      ((op Shift_right_logical) (a 10000000000000000000000010000000)
       (b 00000000000000000000000000000101)
       (result 00000100000000000000000000000100))
      ((op Shift_right_arithmetic) (a 10000000000000000000000010000000)
       (b 00000000000000000000000000000101)
       (result 11111100000000000000000000000100)) |}]
  ;;
end
