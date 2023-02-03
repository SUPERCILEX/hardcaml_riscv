open! Core
open Hardcaml

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
  let opcode = i.instruction.:[6, 0] in
  let funct3 = i.instruction.:[14, 12] in
  let funct7 = i.instruction.:[31, 25] in
  let set_instruction i =
    Instruction.Binary.(Of_always.assign instruction (Of_signal.of_enum i))
  in
  Always.(
    compile
      [ rd <-- i.instruction.:[11, 7]
      ; rs1 <-- i.instruction.:[19, 15]
      ; rs2 <-- i.instruction.:[24, 20]
      ; switch
          opcode
          [ ( of_bit_string "0110111"
            , [ set_instruction Lui; immediate <-- i.instruction.:[31, 12] @: zero 12 ] )
          ; ( of_bit_string "0010111"
            , [ set_instruction Auipc; immediate <-- i.instruction.:[31, 12] @: zero 12 ]
            )
          ; ( of_bit_string "1101111"
            , [ set_instruction Jal
              ; immediate
                <-- sresize
                      (i.instruction.:(31)
                      @: i.instruction.:+[12, Some 8]
                      @: i.instruction.:(20)
                      @: i.instruction.:+[21, Some 10]
                      @: gnd)
                      32
              ] )
          ; ( of_bit_string "1100111"
            , [ when_
                  (funct3 ==: of_bit_string "000")
                  [ set_instruction Jalr
                  ; immediate <-- sresize i.instruction.:+[20, Some 12] 32
                  ]
              ] )
          ; ( of_bit_string "1100011"
            , [ immediate
                <-- sresize
                      (i.instruction.:(31)
                      @: i.instruction.:(7)
                      @: i.instruction.:+[25, Some 6]
                      @: i.instruction.:-[Some 11, 4]
                      @: gnd)
                      32
              ; switch
                  funct3
                  [ of_bit_string "000", [ set_instruction Beq ]
                  ; of_bit_string "001", [ set_instruction Bne ]
                  ; of_bit_string "100", [ set_instruction Blt ]
                  ; of_bit_string "101", [ set_instruction Bge ]
                  ; of_bit_string "110", [ set_instruction Bltu ]
                  ; of_bit_string "111", [ set_instruction Bgeu ]
                  ]
              ] )
          ; ( of_bit_string "0000011"
            , [ immediate <-- sresize i.instruction.:+[20, Some 12] 32
              ; switch
                  funct3
                  [ of_bit_string "000", [ set_instruction Lb ]
                  ; of_bit_string "001", [ set_instruction Lh ]
                  ; of_bit_string "010", [ set_instruction Lw ]
                  ; of_bit_string "100", [ set_instruction Lbu ]
                  ; of_bit_string "101", [ set_instruction Lhu ]
                  ]
              ] )
          ; ( of_bit_string "0100011"
            , [ immediate
                <-- sresize (i.instruction.:[31, 25] @: i.instruction.:[11, 7]) 32
              ; switch
                  funct3
                  [ of_bit_string "000", [ set_instruction Sb ]
                  ; of_bit_string "001", [ set_instruction Sh ]
                  ; of_bit_string "010", [ set_instruction Sw ]
                  ]
              ] )
          ; ( of_bit_string "0010011"
            , [ immediate <-- sresize i.instruction.:+[20, Some 12] 32
              ; switch
                  funct3
                  [ of_bit_string "000", [ set_instruction Addi ]
                  ; of_bit_string "010", [ set_instruction Slti ]
                  ; of_bit_string "011", [ set_instruction Sltiu ]
                  ; of_bit_string "100", [ set_instruction Xori ]
                  ; of_bit_string "110", [ set_instruction Ori ]
                  ; of_bit_string "111", [ set_instruction Andi ]
                  ; ( of_bit_string "001"
                    , [ immediate <-- uresize i.instruction.:+[20, Some 5] 32
                      ; switch
                          funct7
                          [ of_bit_string "0000000", [ set_instruction Slli ] ]
                      ] )
                  ; ( of_bit_string "101"
                    , [ immediate <-- uresize i.instruction.:+[20, Some 5] 32
                      ; switch
                          funct7
                          [ of_bit_string "0000000", [ set_instruction Srli ]
                          ; of_bit_string "0100000", [ set_instruction Srai ]
                          ]
                      ] )
                  ]
              ] )
          ; ( of_bit_string "0110011"
            , [ switch
                  funct7
                  [ ( of_bit_string "0000000"
                    , [ switch
                          funct3
                          [ of_bit_string "000", [ set_instruction Add ]
                          ; of_bit_string "001", [ set_instruction Sll ]
                          ; of_bit_string "010", [ set_instruction Slt ]
                          ; of_bit_string "011", [ set_instruction Sltu ]
                          ; of_bit_string "100", [ set_instruction Xor ]
                          ; of_bit_string "101", [ set_instruction Srl ]
                          ; of_bit_string "110", [ set_instruction Or ]
                          ; of_bit_string "111", [ set_instruction And ]
                          ]
                      ] )
                  ; ( of_bit_string "0100000"
                    , [ switch
                          funct3
                          [ of_bit_string "000", [ set_instruction Sub ]
                          ; of_bit_string "101", [ set_instruction Sra ]
                          ]
                      ] )
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

module Tests = struct
  module Simulator = Cyclesim.With_interface (I) (O)
  module Waveform = Hardcaml_waveterm.Waveform

  let test_instructions () =
    String.split_lines
      "lui a0, 0xdead\n\
       auipc a1, 0xbeef\n\
       jal a2, 0xcafe\n\
       jalr a3, a0, 0x69\n\
       beq a4, a1, 0x42\n\
       bne a5, a2, 420\n\
       blt a6, a3, 88\n\
       bge a7, a4, 1234\n\
       bltu t0, a5, -1\n\
       bgeu t1, a6, -1\n\
       lb t2, 42(a7)\n\
       lh t3, 1234(t0)\n\
       lw t4, 0x69(t1)\n\
       lbu t5, -1(t2)\n\
       lhu t6, -1(t3)\n\
       sb t2, 42(a7)\n\
       sh t3, 1234(t0)\n\
       sw t4, 0x69(t1)\n\
       addi s0, t4, 987\n\
       slti s1, t5, 574\n\
       sltiu s2, t6, -1\n\
       xori s3, s0, 296\n\
       ori s4, s1, 420\n\
       andi s5, s2, 698\n\
       slli s5, s2, 8\n\
       srli s5, s2, 21\n\
       srai s5, s2, 17\n\
       add s6, s5, s2\n\
       sub s6, s5, s2\n\
       sll s6, s5, s2\n\
       slt s6, s5, s2\n\
       sltu s6, s5, s2\n\
       xor s6, s5, s2\n\
       srl s6, s5, s2\n\
       sra s6, s5, s2\n\
       or s6, s5, s2\n\
       and s6, s5, s2"
  ;;

  let test_instruction_bytes () =
    String.split
      ~on:' '
      "37 d5 ea 0d 97 f5 ee 0b 6f 06 fe ca e7 06 95 06 63 14 b7 00 63 84 c7 00 63 54 d8 \
       00 63 c4 e8 00 63 f4 f2 00 63 64 03 01 83 83 a8 02 03 9e 22 4d 83 2e 93 06 03 cf \
       f3 ff 83 5f fe ff 23 85 78 02 23 99 c2 4d a3 24 d3 07 13 84 be 3d 93 24 ef 23 13 \
       b9 ff ff 93 49 84 12 13 ea 44 1a 93 7a a9 2b 93 1a 89 00 93 5a 59 01 93 5a 19 41 \
       33 8b 2a 01 33 8b 2a 41 33 9b 2a 01 33 ab 2a 01 33 bb 2a 01 33 cb 2a 01 33 db 2a \
       01 33 db 2a 41 33 eb 2a 01 33 fb 2a 01"
    |> List.chunks_of ~length:4
    |> List.map ~f:(fun bytes -> String.concat ~sep:"" (List.rev bytes))
    |> List.map ~f:(Bits.of_hex ~width:32)
  ;;

  let test_bench (sim : (_ I.t, _ O.t) Cyclesim.t) =
    let inputs, outputs = Cyclesim.inputs sim, Cyclesim.outputs sim in
    let print_state instruction =
      let instruction_bits = !(inputs.instruction) in
      let id = Instruction.Binary.sim_get_exn outputs.instruction in
      let rd = Bits.to_int !(outputs.rd) in
      let rs1 = Bits.to_int !(outputs.rs1) in
      let rs2 = Bits.to_int !(outputs.rs2) in
      let immediate = Bits.to_int !(outputs.immediate) in
      let immediate =
        if Option.is_some (String.substr_index instruction ~pattern:"0x")
        then Printf.sprintf "0x%x" immediate
        else Int.to_string immediate
      in
      Stdio.print_s
        [%message
          (instruction : string)
            (instruction_bits : Bits.t)
            (id : Instruction.RV32I.t)
            (rd : int)
            (rs1 : int)
            (rs2 : int)
            (immediate : string)];
      Stdio.print_endline ""
    in
    List.iter2_exn
      (test_instruction_bytes ())
      (test_instructions ())
      ~f:(fun bytes instruction ->
      inputs.instruction := bytes;
      Cyclesim.cycle sim;
      print_state instruction)
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
      ((instruction "lui a0, 0xdead")
       (instruction_bits 00001101111010101101010100110111) (id Lui) (rd 10)
       (rs1 21) (rs2 30) (immediate 0xdead000))

      ((instruction "auipc a1, 0xbeef")
       (instruction_bits 00001011111011101111010110010111) (id Auipc) (rd 11)
       (rs1 29) (rs2 30) (immediate 0xbeef000))

      ((instruction "jal a2, 0xcafe")
       (instruction_bits 11001010111111100000011001101111) (id Jal) (rd 12)
       (rs1 28) (rs2 15) (immediate 0xfffe0cae))

      ((instruction "jalr a3, a0, 0x69")
       (instruction_bits 00000110100101010000011011100111) (id Jalr) (rd 13)
       (rs1 10) (rs2 9) (immediate 0x69))

      ((instruction "beq a4, a1, 0x42")
       (instruction_bits 00000000101101110001010001100011) (id Bne) (rd 8)
       (rs1 14) (rs2 11) (immediate 0x8))

      ((instruction "bne a5, a2, 420")
       (instruction_bits 00000000110001111000010001100011) (id Beq) (rd 8)
       (rs1 15) (rs2 12) (immediate 8))

      ((instruction "blt a6, a3, 88")
       (instruction_bits 00000000110110000101010001100011) (id Bge) (rd 8)
       (rs1 16) (rs2 13) (immediate 8))

      ((instruction "bge a7, a4, 1234")
       (instruction_bits 00000000111010001100010001100011) (id Blt) (rd 8)
       (rs1 17) (rs2 14) (immediate 8))

      ((instruction "bltu t0, a5, -1")
       (instruction_bits 00000000111100101111010001100011) (id Bgeu) (rd 8)
       (rs1 5) (rs2 15) (immediate 8))

      ((instruction "bgeu t1, a6, -1")
       (instruction_bits 00000001000000110110010001100011) (id Bltu) (rd 8)
       (rs1 6) (rs2 16) (immediate 8))

      ((instruction "lb t2, 42(a7)")
       (instruction_bits 00000010101010001000001110000011) (id Lb) (rd 7) (rs1 17)
       (rs2 10) (immediate 42))

      ((instruction "lh t3, 1234(t0)")
       (instruction_bits 01001101001000101001111000000011) (id Lh) (rd 28)
       (rs1 5) (rs2 18) (immediate 1234))

      ((instruction "lw t4, 0x69(t1)")
       (instruction_bits 00000110100100110010111010000011) (id Lw) (rd 29)
       (rs1 6) (rs2 9) (immediate 0x69))

      ((instruction "lbu t5, -1(t2)")
       (instruction_bits 11111111111100111100111100000011) (id Lbu) (rd 30)
       (rs1 7) (rs2 31) (immediate 4294967295))

      ((instruction "lhu t6, -1(t3)")
       (instruction_bits 11111111111111100101111110000011) (id Lhu) (rd 31)
       (rs1 28) (rs2 31) (immediate 4294967295))

      ((instruction "sb t2, 42(a7)")
       (instruction_bits 00000010011110001000010100100011) (id Sb) (rd 10)
       (rs1 17) (rs2 7) (immediate 42))

      ((instruction "sh t3, 1234(t0)")
       (instruction_bits 01001101110000101001100100100011) (id Sh) (rd 18)
       (rs1 5) (rs2 28) (immediate 1234))

      ((instruction "sw t4, 0x69(t1)")
       (instruction_bits 00000111110100110010010010100011) (id Sw) (rd 9) (rs1 6)
       (rs2 29) (immediate 0x69))

      ((instruction "addi s0, t4, 987")
       (instruction_bits 00111101101111101000010000010011) (id Addi) (rd 8)
       (rs1 29) (rs2 27) (immediate 987))

      ((instruction "slti s1, t5, 574")
       (instruction_bits 00100011111011110010010010010011) (id Slti) (rd 9)
       (rs1 30) (rs2 30) (immediate 574))

      ((instruction "sltiu s2, t6, -1")
       (instruction_bits 11111111111111111011100100010011) (id Sltiu) (rd 18)
       (rs1 31) (rs2 31) (immediate 4294967295))

      ((instruction "xori s3, s0, 296")
       (instruction_bits 00010010100001000100100110010011) (id Xori) (rd 19)
       (rs1 8) (rs2 8) (immediate 296))

      ((instruction "ori s4, s1, 420")
       (instruction_bits 00011010010001001110101000010011) (id Ori) (rd 20)
       (rs1 9) (rs2 4) (immediate 420))

      ((instruction "andi s5, s2, 698")
       (instruction_bits 00101011101010010111101010010011) (id Andi) (rd 21)
       (rs1 18) (rs2 26) (immediate 698))

      ((instruction "slli s5, s2, 8")
       (instruction_bits 00000000100010010001101010010011) (id Slli) (rd 21)
       (rs1 18) (rs2 8) (immediate 8))

      ((instruction "srli s5, s2, 21")
       (instruction_bits 00000001010110010101101010010011) (id Srli) (rd 21)
       (rs1 18) (rs2 21) (immediate 21))

      ((instruction "srai s5, s2, 17")
       (instruction_bits 01000001000110010101101010010011) (id Srai) (rd 21)
       (rs1 18) (rs2 17) (immediate 17))

      ((instruction "add s6, s5, s2")
       (instruction_bits 00000001001010101000101100110011) (id Add) (rd 22)
       (rs1 21) (rs2 18) (immediate 0))

      ((instruction "sub s6, s5, s2")
       (instruction_bits 01000001001010101000101100110011) (id Sub) (rd 22)
       (rs1 21) (rs2 18) (immediate 0))

      ((instruction "sll s6, s5, s2")
       (instruction_bits 00000001001010101001101100110011) (id Sll) (rd 22)
       (rs1 21) (rs2 18) (immediate 0))

      ((instruction "slt s6, s5, s2")
       (instruction_bits 00000001001010101010101100110011) (id Slt) (rd 22)
       (rs1 21) (rs2 18) (immediate 0))

      ((instruction "sltu s6, s5, s2")
       (instruction_bits 00000001001010101011101100110011) (id Sltu) (rd 22)
       (rs1 21) (rs2 18) (immediate 0))

      ((instruction "xor s6, s5, s2")
       (instruction_bits 00000001001010101100101100110011) (id Xor) (rd 22)
       (rs1 21) (rs2 18) (immediate 0))

      ((instruction "srl s6, s5, s2")
       (instruction_bits 00000001001010101101101100110011) (id Srl) (rd 22)
       (rs1 21) (rs2 18) (immediate 0))

      ((instruction "sra s6, s5, s2")
       (instruction_bits 01000001001010101101101100110011) (id Sra) (rd 22)
       (rs1 21) (rs2 18) (immediate 0))

      ((instruction "or s6, s5, s2")
       (instruction_bits 00000001001010101110101100110011) (id Or) (rd 22)
       (rs1 21) (rs2 18) (immediate 0))

      ((instruction "and s6, s5, s2")
       (instruction_bits 00000001001010101111101100110011) (id And) (rd 22)
       (rs1 21) (rs2 18) (immediate 0)) |}]
  ;;
end
