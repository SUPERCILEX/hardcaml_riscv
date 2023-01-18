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
  let reg_spec () = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let sm =
    let state = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    Always.State_machine.create (module State) state
  in
  let num = Always.Variable.reg ~width:(width i.num) (reg_spec ()) in
  let num_ones = Always.Variable.reg ~width:4 (reg_spec ()) in
  let _ = num.value -- "num_internal" in
  let _ = sm.current -- "state" in
  Always.(
    compile
      [ sm.switch
          [ ( Idle
            , [ num_ones <-- zero (width num_ones.value)
              ; if_ i.start [ sm.set_next Compute ] [ num <-- i.num ]
              ] )
          ; ( Compute
            , [ num <-- srl num.value 1
              ; when_ (num.value ==:. 0) [ sm.set_next Done ]
              ; when_
                  (num.value <>:. 0 &&: lsb num.value)
                  [ num_ones <-- num_ones.value +:. 1 ]
              ] )
          ; Done, [ when_ ~:(i.start) [ sm.set_next Idle ] ]
          ]
      ]);
  { O.num_ones = num_ones.value; done_ = sm.is Done }
;;

let circuit () = Circuit.create_with_interface (module I) (module O) ~name:"cpu" create

module Tests = struct
  open Core
  module Simulator = Cyclesim.With_interface (I) (O)
  module Waveform = Hardcaml_waveterm.Waveform

  let test_bench (sim : (_ I.t, _ O.t) Cyclesim.t) =
    let inputs, outputs = Cyclesim.inputs sim, Cyclesim.outputs sim in
    let print_state_and_outputs () =
      let done_ = Bits.is_vdd !(outputs.done_) in
      let result = Bits.to_int !(outputs.num_ones) in
      Stdio.print_s [%message (done_ : bool) (result : int)]
    in
    let reset () =
      Cyclesim.reset sim;
      inputs.clear := Bits.vdd;
      Cyclesim.cycle sim;
      inputs.clear := Bits.gnd;
      print_state_and_outputs ()
    in
    let init bits =
      inputs.num := Bits.of_bit_string bits;
      Cyclesim.cycle sim;
      inputs.start := Bits.vdd;
      Cyclesim.cycle sim;
      print_state_and_outputs ()
    in
    let run () =
      inputs.start := Bits.gnd;
      for _ = 0 to 11 do
        Cyclesim.cycle sim;
        print_state_and_outputs ()
      done
    in
    reset ();
    init "10110001";
    run ()
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
      (output_rules |> O.to_list)
      @ [ port_name_is
            "state"
            ~wave_format:
              (Index
                 (List.map State.all ~f:(fun t -> State.sexp_of_t t |> Sexp.to_string)))
        ]
    in
    let waves =
      let sim = Simulator.create ~config:Cyclesim.Config.trace_all create in
      let waves, sim = Waveform.create sim in
      test_bench sim;
      waves
    in
    Waveform.print
      waves
      ~display_height:25
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
      │                  ││────────────────────────────────┬───────────────────────────────────────────────────────────────────────┬───────┬───────────────│
      │state             ││ Idle                           │Compute                                                                │Done   │Idle           │
      │                  ││────────────────────────────────┴───────────────────────────────────────────────────────────────────────┴───────┴───────────────│
      │                  ││────────────────────────┬───────────────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────────────────────┬───────│
      │num_internal      ││ 00                     │B1             │58     │2C     │16     │0B     │05     │02     │01     │00                     │B1     │
      │                  ││────────────────────────┴───────────────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────────────────────┴───────│
      │vdd               ││        ┌───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
      │                  ││────────┘                                                                                                                       │
      │                  ││                                                                                                                                │
      └──────────────────┘└────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘ |}]
  ;;
end
