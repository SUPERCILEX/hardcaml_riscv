open Hardcaml

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; num : 'a [@bits 8]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { done_ : 'a
    ; num_ones : 'a [@bits 4]
    }
  [@@deriving sexp_of, hardcaml]
end

module State = struct
  type t =
    | Idle
    | Compute
    | Done
  [@@deriving sexp_of, compare, enumerate]
end

let create (i : _ I.t) =
  let open Signal in
  let c_init_regs = wire 1 -- "c_init_regs" in
  let c_load_num = wire 1 -- "c_load_num" in
  let c_shift = wire 1 -- "c_shift" in
  let c_inc = wire 1 -- "c_inc" in
  let s_num_zero = wire 1 -- "s_num_zero" in
  let s_bit_high = wire 1 -- "s_bit_high" in
  let ns = wire 2 -- "ns" in
  let ps = reg (Reg_spec.create ~clock:i.clock ~clear:i.clear ()) ns -- "ps" in
  let num_ones_d = wire 4 -- "num_ones_d" in
  let num_ones = reg (Reg_spec.create ~clock:i.clock ~clear:i.clear ()) num_ones_d in
  let done_ = ps ==:. 2 in
  let num_internal_d = wire (width i.num) -- "num_internal" in
  let num_internal =
    reg (Reg_spec.create ~clock:i.clock ~clear:i.clear ()) num_internal_d
  in
  c_init_regs <== (ps ==:. 0);
  c_load_num <== (ps ==:. 0 &&: ~:(i.start));
  c_shift <== (ps ==:. 1);
  c_inc <== (ps ==:. 1 &&: ~:s_num_zero &&: s_bit_high);
  s_num_zero <== (num_internal ==:. 0);
  s_bit_high <== lsb num_internal;
  ns
  <== mux
        ps
        [ mux2 i.start (uresize vdd 2) (uresize gnd 2)
        ; mux2 s_num_zero (of_decimal_string ~width:2 "2") (uresize vdd 2)
        ; mux2 i.start (of_decimal_string ~width:2 "2") (uresize gnd 2)
        ; uresize gnd 2
        ];
  num_ones_d <== mux2 c_init_regs (uresize gnd 4) (mux2 c_inc (num_ones +:. 1) num_ones);
  num_internal_d
  <== mux2 c_load_num i.num (mux2 c_shift (srl num_internal 1) num_internal);
  { O.num_ones; done_ }
;;

let circuit () = Circuit.create_with_interface (module I) (module O) ~name:"cpu" create

module Tests = struct
  open Core
  module Simulator = Cyclesim.With_interface (I) (O)
  module Waveform = Hardcaml_waveterm.Waveform

  let test_bench (sim : (_ I.t, _ O.t) Cyclesim.t) =
    let inputs, outputs = Cyclesim.inputs sim, Cyclesim.outputs sim in
    let print_state_and_outputs () =
      (* let state =
        List.nth_exn States.all (Bits.to_int !(outputs.state))
      in *)
      let done_ = Bits.is_vdd !(outputs.done_) in
      let result = Bits.to_int !(outputs.num_ones) in
      Stdio.print_s [%message (done_ : bool) (result : int)]
    in
    (* Start by resetting simulation and clearing the circuit. *)
    Cyclesim.reset sim;
    inputs.clear := Bits.vdd;
    Cyclesim.cycle sim;
    inputs.clear := Bits.gnd;
    (* Cycle 0 *)
    print_state_and_outputs ();
    (* Cycle 1 *)
    inputs.num := Bits.of_bit_string "10110001";
    Cyclesim.cycle sim;
    inputs.start := Bits.vdd;
    Cyclesim.cycle sim;
    print_state_and_outputs ();
    inputs.start := Bits.gnd;
    for _ = 0 to 11 do
      Cyclesim.cycle sim;
      print_state_and_outputs ()
    done
  ;;

  let%expect_test "Simple" =
    let open Hardcaml_waveterm.Display_rule in
    let input_rules =
      I.(map port_names ~f:(port_name_is ~wave_format:(Bit_or Unsigned_int)) |> to_list)
    in
    let output_rules =
      O.(map port_names ~f:(port_name_is ~wave_format:(Bit_or Unsigned_int)))
    in
    let output_rules =
      (* { output_rules with
        O.state =
          port_name_is
            "state"
            ~wave_format:
              (Index
                 (List.map States.all ~f:(fun t -> States.sexp_of_t t |> Sexp.to_string)))
      } *)
      output_rules |> O.to_list
    in
    let waves =
      let sim = Simulator.create ~config:Cyclesim.Config.trace_all create in
      let waves, sim = Waveform.create sim in
      test_bench sim;
      waves
    in
    Waveform.print
      waves
      ~display_height:45
      ~display_width:150
      ~display_rules:(input_rules @ output_rules @ [ default ]);
    [%expect
      {|
      ((done_ false) (result 0))
      ((done_ false) (result 0))
      ((done_ false) (result 1))
      ((done_ false) (result 1))
      ((done_ false) (result 1))
      ((done_ false) (result 1))
      ((done_ false) (result 2))
      ((done_ false) (result 3))
      ((done_ false) (result 3))
      ((done_ false) (result 4))
      ((done_ true) (result 4))
      ((done_ false) (result 4))
      ((done_ false) (result 0))
      ((done_ false) (result 0))
      ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
      │clock             ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   │
      │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───│
      │clear             ││        ┌───────┐                                                                                                               │
      │                  ││────────┘       └───────────────────────────────────────────────────────────────────────────────────────────────────────────────│
      │start             ││                        ┌───────┐                                                                                               │
      │                  ││────────────────────────┘       └───────────────────────────────────────────────────────────────────────────────────────────────│
      │                  ││────────────────┬───────────────────────────────────────────────────────────────────────────────────────────────────────────────│
      │num               ││ 0              │177                                                                                                            │
      │                  ││────────────────┴───────────────────────────────────────────────────────────────────────────────────────────────────────────────│
      │done_             ││                                                                                                        ┌───────┐               │
      │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────┘       └───────────────│
      │                  ││────────────────────────────────────────┬───────────────────────────────┬───────┬───────────────┬───────────────────────┬───────│
      │num_ones          ││ 0                                      │1                              │2      │3              │4                      │0      │
      │                  ││────────────────────────────────────────┴───────────────────────────────┴───────┴───────────────┴───────────────────────┴───────│
      │c_inc             ││                                ┌───────┐                       ┌───────────────┐       ┌───────┐                               │
      │                  ││────────────────────────────────┘       └───────────────────────┘               └───────┘       └───────────────────────────────│
      │c_init_regs       ││        ┌───────────────────────┐                                                                               ┌───────────────│
      │                  ││────────┘                       └───────────────────────────────────────────────────────────────────────────────┘               │
      │c_load_num        ││        ┌───────────────┐                                                                                       ┌───────────────│
      │                  ││────────┘               └───────────────────────────────────────────────────────────────────────────────────────┘               │
      │c_shift           ││                                ┌───────────────────────────────────────────────────────────────────────┐                       │
      │                  ││────────────────────────────────┘                                                                       └───────────────────────│
      │                  ││────────────────────────┬───────────────────────────────────────────────────────────────────────┬───────┬───────────────────────│
      │ns                ││ 0                      │1                                                                      │2      │0                      │
      │                  ││────────────────────────┴───────────────────────────────────────────────────────────────────────┴───────┴───────────────────────│
      │                  ││────────────────┬───────────────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────────────────────┬───────────────│
      │num_internal      ││ 00             │B1             │58     │2C     │16     │0B     │05     │02     │01     │00                     │B1             │
      │                  ││────────────────┴───────────────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────────────────────┴───────────────│
      │                  ││────────────────────────────────┬───────────────────────────────┬───────┬───────────────┬───────────────────────┬───────────────│
      │num_ones_d        ││ 0                              │1                              │2      │3              │4                      │0              │
      │                  ││────────────────────────────────┴───────────────────────────────┴───────┴───────────────┴───────────────────────┴───────────────│
      │                  ││────────────────────────────────┬───────────────────────────────────────────────────────────────────────┬───────┬───────────────│
      │ps                ││ 0                              │1                                                                      │2      │0              │
      │                  ││────────────────────────────────┴───────────────────────────────────────────────────────────────────────┴───────┴───────────────│
      │s_bit_high        ││                        ┌───────────────┐                       ┌───────────────┐       ┌───────┐                       ┌───────│
      │                  ││────────────────────────┘               └───────────────────────┘               └───────┘       └───────────────────────┘       │
      │s_num_zero        ││        ┌───────────────┐                                                                       ┌───────────────────────┐       │
      │                  ││────────┘               └───────────────────────────────────────────────────────────────────────┘                       └───────│
      │vdd               ││        ┌───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
      │                  ││────────┘                                                                                                                       │
      │                  ││                                                                                                                                │
      │                  ││                                                                                                                                │
      │                  ││                                                                                                                                │
      └──────────────────┘└────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘ |}]
  ;;
end
