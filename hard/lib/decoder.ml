open Hardcaml

module Instruction = struct
  module RV32I = struct
    type t =
      | Lui (* Load upper immediate *)
      | Auipc (* Add upper immediate to program counter *)
      | Jal (* Jump and link *)
      | Jalr (* Jump and link register *)
      | Beq (* Branch if equal *)
      | Bne (* Branch if not equal *)
      | Blt (* Branch if less than *)
      | Bge (* Branch if greater than or equal *)
      | Bltu (* Branch if less than unsigned *)
      | Bgeu (* Branch if greater than or equal unsigned *)
      | Lb (* Load byte *)
      | Lh (* Load halfword *)
      | Lw (* Load word *)
      | Lbu (* Load byte unsigned *)
      | Lhu (* Load halfword unsigned *)
      | Sb (* Store byte *)
      | Sh (* Store halfword *)
      | Sw (* Store word *)
      | Addi (* Add immediate *)
      | Slti (* Set less than immediate *)
      | Sltiu (* Set less than immediate unsigned *)
      | Xori (* Exclusive or immediate *)
      | Ori (* Or immediate *)
      | Andi (* And immediate *)
      | Slli (* Shift left logical immediate *)
      | Srli (* Shift right logical immediate *)
      | Srai (* Shift right arithmetic immediate *)
      | Add (* Add *)
      | Sub (* Subtract *)
      | Sll (* Shift left logical *)
      | Slt (* Set less than *)
      | Sltu (* Set less than unsigned *)
      | Xor (* Exclusive or *)
      | Srl (* Shift right logical *)
      | Sra (* Shift right arithmetic *)
      | Or (* Or *)
      | And (* And *)
    [@@deriving sexp_of, compare, enumerate]
  end

  include Hardcaml.Interface.Make_enums (RV32I)
end

module I = struct
  type 'a t = { instruction : 'a [@bits 32] } [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { instruction : 'a Instruction.Binary.t
    ; rd : 'a [@bits 5]
    ; rs1 : 'a [@bits 5]
    ; rs2 : 'a [@bits 5]
    ; immediate : 'a [@bits 32]
    }
  [@@deriving sexp_of, hardcaml]
end

let create (scope : Scope.t) (i : _ I.t) =
  let open Signal in
  let instruction = Instruction.Binary.Of_always.wire ones in
  let rd = Always.Variable.wire ~default:(zero 5) in
  let rs1 = Always.Variable.wire ~default:(zero 5) in
  let rs2 = Always.Variable.wire ~default:(zero 5) in
  let immediate = Always.Variable.wire ~default:(zero 32) in
  let _debugging =
    let ( -- ) = Scope.naming scope in
    let _ = rd.value -- "output_register" in
    let _ = rs1.value -- "input_register1" in
    let _ = rs2.value -- "input_register2" in
    let _ = immediate.value -- "parsed_immediate" in
    ()
  in
  let opcode_slice = 6, 0 in
  let rd_slice = 11, 7 in
  let funct3_slice = 14, 12 in
  let rs1_slice = 19, 15 in
  let rs2_slice = 24, 20 in
  Always.(
    compile
      [ switch
          i.instruction.:[opcode_slice]
          [ ( of_bit_string "0110111"
            , [ Instruction.Binary.Of_always.assign
                  instruction
                  (Instruction.Binary.Of_signal.of_enum Lui)
              ; rd <-- i.instruction.:[rd_slice]
              ; immediate <-- i.instruction.:[31, 12] @: zero 12
              ] )
          ; ( of_bit_string "0010111"
            , [ Instruction.Binary.Of_always.assign
                  instruction
                  (Instruction.Binary.Of_signal.of_enum Auipc)
              ; rd <-- i.instruction.:[rd_slice]
              ; immediate <-- i.instruction.:[31, 12] @: zero 12
              ] )
          ; ( of_bit_string "1101111"
            , [ Instruction.Binary.Of_always.assign
                  instruction
                  (Instruction.Binary.Of_signal.of_enum Jal)
              ; rd <-- i.instruction.:[rd_slice]
              ; immediate
                <-- zero 11
                    @: i.instruction.:(31)
                    @: i.instruction.:+[8, Some 12]
                    @: i.instruction.:(20)
                    @: i.instruction.:+[9, Some 21]
                    @: gnd
              ] )
          ; ( of_bit_string "1100111"
            , [ when_
                  (i.instruction.:[funct3_slice] ==: of_bit_string "000")
                  [ Instruction.Binary.Of_always.assign
                      instruction
                      (Instruction.Binary.Of_signal.of_enum Jalr)
                  ; rd <-- i.instruction.:[rd_slice]
                  ; rs1 <-- i.instruction.:[rs1_slice]
                  ; immediate <-- zero 20 @: i.instruction.:[31, 20]
                  ]
              ] )
          ; ( of_bit_string "1100011"
            , [ rs1 <-- i.instruction.:[rs1_slice]
              ; rs2 <-- i.instruction.:[rs2_slice]
              ; immediate
                <-- zero 19
                    @: i.instruction.:(31)
                    @: i.instruction.:(7)
                    @: i.instruction.:+[6, Some 25]
                    @: i.instruction.:-[Some 11, 4]
                    @: gnd
              ; switch
                  i.instruction.:[funct3_slice]
                  Instruction.Binary.
                    [ ( of_bit_string "000"
                      , [ Of_always.assign instruction (Of_signal.of_enum Beq) ] )
                    ; ( of_bit_string "001"
                      , [ Of_always.assign instruction (Of_signal.of_enum Bne) ] )
                    ; ( of_bit_string "100"
                      , [ Of_always.assign instruction (Of_signal.of_enum Blt) ] )
                    ; ( of_bit_string "101"
                      , [ Of_always.assign instruction (Of_signal.of_enum Bge) ] )
                    ; ( of_bit_string "110"
                      , [ Of_always.assign instruction (Of_signal.of_enum Bltu) ] )
                    ; ( of_bit_string "111"
                      , [ Of_always.assign instruction (Of_signal.of_enum Bgeu) ] )
                    ]
              ] )
          ; ( of_bit_string "0000011"
            , [ rd <-- i.instruction.:[rd_slice]
              ; rs1 <-- i.instruction.:[rs1_slice]
              ; immediate <-- zero 20 @: i.instruction.:[31, 20]
              ; switch
                  i.instruction.:[funct3_slice]
                  Instruction.Binary.
                    [ ( of_bit_string "000"
                      , [ Of_always.assign instruction (Of_signal.of_enum Lb) ] )
                    ; ( of_bit_string "001"
                      , [ Of_always.assign instruction (Of_signal.of_enum Lh) ] )
                    ; ( of_bit_string "010"
                      , [ Of_always.assign instruction (Of_signal.of_enum Lw) ] )
                    ; ( of_bit_string "100"
                      , [ Of_always.assign instruction (Of_signal.of_enum Lbu) ] )
                    ; ( of_bit_string "101"
                      , [ Of_always.assign instruction (Of_signal.of_enum Lhu) ] )
                    ]
              ] )
          ; ( of_bit_string "0100011"
            , [ rs1 <-- i.instruction.:[rs1_slice]
              ; rs2 <-- i.instruction.:[rs2_slice]
              ; immediate <-- zero 20 @: i.instruction.:[31, 25] @: i.instruction.:[11, 7]
              ; switch
                  i.instruction.:[funct3_slice]
                  Instruction.Binary.
                    [ ( of_bit_string "000"
                      , [ Of_always.assign instruction (Of_signal.of_enum Sb) ] )
                    ; ( of_bit_string "001"
                      , [ Of_always.assign instruction (Of_signal.of_enum Sh) ] )
                    ; ( of_bit_string "010"
                      , [ Of_always.assign instruction (Of_signal.of_enum Sw) ] )
                    ]
              ] )
          ; ( of_bit_string "0010011"
            , [ rs1 <-- i.instruction.:[rs1_slice]
              ; rs2 <-- i.instruction.:[rs2_slice]
              ; immediate <-- zero 20 @: i.instruction.:[31, 25] @: i.instruction.:[11, 7]
              ; switch
                  i.instruction.:[funct3_slice]
                  Instruction.Binary.
                    [ ( of_bit_string "000"
                      , [ Of_always.assign instruction (Of_signal.of_enum Sb) ] )
                    ; ( of_bit_string "001"
                      , [ Of_always.assign instruction (Of_signal.of_enum Sh) ] )
                    ; ( of_bit_string "010"
                      , [ Of_always.assign instruction (Of_signal.of_enum Sw) ] )
                    ]
              ] )
          ]
      ]);
  { O.instruction = Instruction.Binary.Of_always.value instruction
  ; rd = rd.value
  ; rs1 = rs1.value
  ; rs2 = rs2.value
  ; immediate = immediate.value
  }
;;

let circuit scope =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"decoder" create
;;

(* module Tests = struct
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
      | And | Or | Xor -> print_state_bits ()
    in
    let bit_num = Bits.of_int ~width:Parameters.word_size in
    run Add (bit_num 69) (bit_num 42);
    run Sub (bit_num 69) (bit_num 42);
    run And (bit_num 69) (bit_num 42);
    run Or (bit_num 69) (bit_num 42);
    run Xor (bit_num 69) (bit_num 438)
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
       (result 00000000000000000000000111110011)) |}]
  ;;
end *)
