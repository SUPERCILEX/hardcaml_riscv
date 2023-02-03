open! Core
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
    { done_ : 'a [@rtlname "done"]
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

let create (scope : Scope.t) (i : _ I.t) =
  let open Signal in
  let ( -- ) = Scope.naming scope in
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let sm = Always.State_machine.create (module State) spec in
  let num = Always.Variable.reg ~width:(width i.num) spec in
  let num_ones = Always.Variable.reg ~width:4 spec in
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

let circuit scope =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"playground" create
;;

let root scope =
  Circuit.create_with_interface (module I) (module O) ~name:"cpu" (circuit scope)
;;

module Tests = struct
  open Core
  module Simulator = Cyclesim.With_interface (I) (O)
  module Waveform = Hardcaml_waveterm.Waveform

  let test_bench (sim : (_ I.t, _ O.t) Cyclesim.t) =
    let inputs, outputs = Cyclesim.inputs sim, Cyclesim.outputs sim in
    let print_state_and_outputs () =
      let state =
        List.nth_exn
          State.all
          (Bits.to_int
             !(List.Assoc.find_exn
                 (Cyclesim.internal_ports sim)
                 "state"
                 ~equal:String.equal))
      in
      Stdio.print_s
        ([%sexp_of: State.t * int O.t]
           (state, O.map outputs ~f:(fun p -> Bits.to_int !p)))
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

  let waves () =
    let scope = Scope.create ~flatten_design:true () in
    let sim = Simulator.create ~config:Cyclesim.Config.trace_all (create scope) in
    let waves, sim = Waveform.create sim in
    test_bench sim;
    waves
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
    Waveform.print
      (waves ())
      ~display_height:25
      ~display_width:150
      ~display_rules:(input_rules @ output_rules @ [ default ]);
    [%expect
      {|
      (Idle ((done_ 0) (num_ones 0)))
      (Idle ((done_ 0) (num_ones 0)))
      (Compute ((done_ 0) (num_ones 1)))
      (Compute ((done_ 0) (num_ones 1)))
      (Compute ((done_ 0) (num_ones 1)))
      (Compute ((done_ 0) (num_ones 1)))
      (Compute ((done_ 0) (num_ones 2)))
      (Compute ((done_ 0) (num_ones 3)))
      (Compute ((done_ 0) (num_ones 3)))
      (Compute ((done_ 0) (num_ones 4)))
      (Compute ((done_ 1) (num_ones 4)))
      (Done ((done_ 0) (num_ones 4)))
      (Idle ((done_ 0) (num_ones 0)))
      (Idle ((done_ 0) (num_ones 0)))
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
      │done              ││                                                                                                        ┌───────┐               │
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
