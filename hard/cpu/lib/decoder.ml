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

let create (scope : Scope.t) ({ instruction = raw_instruction } : _ I.t) =
  let open Signal in
  let ({ O.instruction; rd; rs1; rs2; immediate } as out) =
    { (O.Of_always.wire zero) with
      instruction =
        Instruction.Binary.(Of_always.wire (fun _ -> Of_signal.of_enum Invalid |> to_raw))
    }
  in
  let _debugging =
    let ( -- ) = Scope.naming scope in
    O.Of_always.value out |> O.Of_signal.apply_names ~naming_op:( -- )
  in
  let opcode = raw_instruction.:[6, 0] in
  let funct3 = raw_instruction.:[14, 12] in
  let funct7 = raw_instruction.:[31, 25] in
  let set_instruction i =
    Instruction.Binary.(Of_always.assign instruction (Of_signal.of_enum i))
  in
  Always.(
    compile
      [ rd <-- raw_instruction.:[11, 7]
      ; rs1 <-- raw_instruction.:[19, 15]
      ; rs2 <-- raw_instruction.:[24, 20]
      ; switch
          opcode
          [ ( of_bit_string "0110111"
            , [ set_instruction Lui; immediate <-- raw_instruction.:[31, 12] @: zero 12 ]
            )
          ; ( of_bit_string "0010111"
            , [ set_instruction Auipc
              ; immediate <-- raw_instruction.:[31, 12] @: zero 12
              ] )
          ; ( of_bit_string "1101111"
            , [ set_instruction Jal
              ; immediate
                <-- sresize
                      (raw_instruction.:(31)
                      @: raw_instruction.:[19, 12]
                      @: raw_instruction.:(20)
                      @: raw_instruction.:[30, 21]
                      @: gnd)
                      32
              ] )
          ; ( of_bit_string "1100111"
            , [ switch
                  funct3
                  [ ( of_bit_string "000"
                    , [ set_instruction Jalr
                      ; immediate <-- sresize raw_instruction.:[31, 20] 32
                      ] )
                  ]
              ] )
          ; ( of_bit_string "1100011"
            , [ immediate
                <-- sresize
                      (raw_instruction.:(31)
                      @: raw_instruction.:(7)
                      @: raw_instruction.:[30, 25]
                      @: raw_instruction.:[11, 8]
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
            , [ immediate <-- sresize raw_instruction.:[31, 20] 32
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
                <-- sresize (raw_instruction.:[31, 25] @: raw_instruction.:[11, 7]) 32
              ; switch
                  funct3
                  [ of_bit_string "000", [ set_instruction Sb ]
                  ; of_bit_string "001", [ set_instruction Sh ]
                  ; of_bit_string "010", [ set_instruction Sw ]
                  ]
              ] )
          ; ( of_bit_string "0010011"
            , [ immediate <-- sresize raw_instruction.:[31, 20] 32
              ; switch
                  funct3
                  ([ of_bit_string "000", [ set_instruction Addi ]
                   ; of_bit_string "010", [ set_instruction Slti ]
                   ; of_bit_string "011", [ set_instruction Sltiu ]
                   ; of_bit_string "100", [ set_instruction Xori ]
                   ; of_bit_string "110", [ set_instruction Ori ]
                   ; of_bit_string "111", [ set_instruction Andi ]
                   ]
                  @
                  let shamt = raw_instruction.:[24, 20] in
                  [ ( of_bit_string "001"
                    , [ immediate <-- uresize shamt 32
                      ; switch
                          funct7
                          [ of_bit_string "0000000", [ set_instruction Slli ] ]
                      ] )
                  ; ( of_bit_string "101"
                    , [ immediate <-- uresize shamt 32
                      ; switch
                          funct7
                          [ of_bit_string "0000000", [ set_instruction Srli ]
                          ; of_bit_string "0100000", [ set_instruction Srai ]
                          ]
                      ] )
                  ])
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
  O.Of_always.value out
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
       and s6, s5, s2\n\
       invalid"
  ;;

  let test_instruction_bytes () =
    String.split
      ~on:' '
      "37 d5 ea 0d 97 f5 ee 0b 6f 06 fe ca e7 06 95 06 63 14 b7 00 63 84 c7 00 63 54 d8 \
       00 63 c4 e8 00 63 f4 f2 00 63 64 03 01 83 83 a8 02 03 9e 22 4d 83 2e 93 06 03 cf \
       f3 ff 83 5f fe ff 23 85 78 02 23 99 c2 4d a3 24 d3 07 13 84 be 3d 93 24 ef 23 13 \
       b9 ff ff 93 49 84 12 13 ea 44 1a 93 7a a9 2b 93 1a 89 00 93 5a 59 01 93 5a 19 41 \
       33 8b 2a 01 33 8b 2a 41 33 9b 2a 01 33 ab 2a 01 33 bb 2a 01 33 cb 2a 01 33 db 2a \
       01 33 db 2a 41 33 eb 2a 01 33 fb 2a 01 00 00 00 00"
    |> List.chunks_of ~length:4
    |> List.map ~f:(fun bytes -> String.concat ~sep:"" (List.rev bytes))
    |> List.map ~f:(Bits.of_hex ~width:32)
  ;;

  let test_bench (sim : (_ I.t, _ O.t) Cyclesim.t) =
    let open Bits in
    let inputs, outputs = Cyclesim.inputs sim, Cyclesim.outputs sim in
    let print_state instruction =
      let id = Instruction.Binary.sim_get_exn outputs.instruction in
      let outputs =
        let outputs = O.map outputs ~f:(( ! ) |> Fn.compose to_int) in
        { (O.map outputs ~f:Int.to_string) with
          immediate =
            (outputs.immediate
            |>
            if Option.is_some (String.substr_index instruction ~pattern:"0x")
            then Printf.sprintf "0x%x"
            else Int.to_string)
        }
      in
      Stdio.print_s
        [%message
          (instruction : string)
            (inputs : Bits.t ref I.t)
            (outputs : string O.t)
            (id : Instruction.RV32I.t)];
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
       (inputs ((instruction 00001101111010101101010100110111)))
       (outputs ((instruction 1) (rd 10) (rs1 21) (rs2 30) (immediate 0xdead000)))
       (id Lui))

      ((instruction "auipc a1, 0xbeef")
       (inputs ((instruction 00001011111011101111010110010111)))
       (outputs ((instruction 2) (rd 11) (rs1 29) (rs2 30) (immediate 0xbeef000)))
       (id Auipc))

      ((instruction "jal a2, 0xcafe")
       (inputs ((instruction 11001010111111100000011001101111)))
       (outputs ((instruction 3) (rd 12) (rs1 28) (rs2 15) (immediate 0xfffe0cae)))
       (id Jal))

      ((instruction "jalr a3, a0, 0x69")
       (inputs ((instruction 00000110100101010000011011100111)))
       (outputs ((instruction 4) (rd 13) (rs1 10) (rs2 9) (immediate 0x69)))
       (id Jalr))

      ((instruction "beq a4, a1, 0x42")
       (inputs ((instruction 00000000101101110001010001100011)))
       (outputs ((instruction 6) (rd 8) (rs1 14) (rs2 11) (immediate 0x8)))
       (id Bne))

      ((instruction "bne a5, a2, 420")
       (inputs ((instruction 00000000110001111000010001100011)))
       (outputs ((instruction 5) (rd 8) (rs1 15) (rs2 12) (immediate 8))) (id Beq))

      ((instruction "blt a6, a3, 88")
       (inputs ((instruction 00000000110110000101010001100011)))
       (outputs ((instruction 8) (rd 8) (rs1 16) (rs2 13) (immediate 8))) (id Bge))

      ((instruction "bge a7, a4, 1234")
       (inputs ((instruction 00000000111010001100010001100011)))
       (outputs ((instruction 7) (rd 8) (rs1 17) (rs2 14) (immediate 8))) (id Blt))

      ((instruction "bltu t0, a5, -1")
       (inputs ((instruction 00000000111100101111010001100011)))
       (outputs ((instruction 10) (rd 8) (rs1 5) (rs2 15) (immediate 8)))
       (id Bgeu))

      ((instruction "bgeu t1, a6, -1")
       (inputs ((instruction 00000001000000110110010001100011)))
       (outputs ((instruction 9) (rd 8) (rs1 6) (rs2 16) (immediate 8))) (id Bltu))

      ((instruction "lb t2, 42(a7)")
       (inputs ((instruction 00000010101010001000001110000011)))
       (outputs ((instruction 11) (rd 7) (rs1 17) (rs2 10) (immediate 42)))
       (id Lb))

      ((instruction "lh t3, 1234(t0)")
       (inputs ((instruction 01001101001000101001111000000011)))
       (outputs ((instruction 12) (rd 28) (rs1 5) (rs2 18) (immediate 1234)))
       (id Lh))

      ((instruction "lw t4, 0x69(t1)")
       (inputs ((instruction 00000110100100110010111010000011)))
       (outputs ((instruction 13) (rd 29) (rs1 6) (rs2 9) (immediate 0x69)))
       (id Lw))

      ((instruction "lbu t5, -1(t2)")
       (inputs ((instruction 11111111111100111100111100000011)))
       (outputs ((instruction 14) (rd 30) (rs1 7) (rs2 31) (immediate 4294967295)))
       (id Lbu))

      ((instruction "lhu t6, -1(t3)")
       (inputs ((instruction 11111111111111100101111110000011)))
       (outputs
        ((instruction 15) (rd 31) (rs1 28) (rs2 31) (immediate 4294967295)))
       (id Lhu))

      ((instruction "sb t2, 42(a7)")
       (inputs ((instruction 00000010011110001000010100100011)))
       (outputs ((instruction 16) (rd 10) (rs1 17) (rs2 7) (immediate 42)))
       (id Sb))

      ((instruction "sh t3, 1234(t0)")
       (inputs ((instruction 01001101110000101001100100100011)))
       (outputs ((instruction 17) (rd 18) (rs1 5) (rs2 28) (immediate 1234)))
       (id Sh))

      ((instruction "sw t4, 0x69(t1)")
       (inputs ((instruction 00000111110100110010010010100011)))
       (outputs ((instruction 18) (rd 9) (rs1 6) (rs2 29) (immediate 0x69)))
       (id Sw))

      ((instruction "addi s0, t4, 987")
       (inputs ((instruction 00111101101111101000010000010011)))
       (outputs ((instruction 19) (rd 8) (rs1 29) (rs2 27) (immediate 987)))
       (id Addi))

      ((instruction "slti s1, t5, 574")
       (inputs ((instruction 00100011111011110010010010010011)))
       (outputs ((instruction 20) (rd 9) (rs1 30) (rs2 30) (immediate 574)))
       (id Slti))

      ((instruction "sltiu s2, t6, -1")
       (inputs ((instruction 11111111111111111011100100010011)))
       (outputs
        ((instruction 21) (rd 18) (rs1 31) (rs2 31) (immediate 4294967295)))
       (id Sltiu))

      ((instruction "xori s3, s0, 296")
       (inputs ((instruction 00010010100001000100100110010011)))
       (outputs ((instruction 22) (rd 19) (rs1 8) (rs2 8) (immediate 296)))
       (id Xori))

      ((instruction "ori s4, s1, 420")
       (inputs ((instruction 00011010010001001110101000010011)))
       (outputs ((instruction 23) (rd 20) (rs1 9) (rs2 4) (immediate 420)))
       (id Ori))

      ((instruction "andi s5, s2, 698")
       (inputs ((instruction 00101011101010010111101010010011)))
       (outputs ((instruction 24) (rd 21) (rs1 18) (rs2 26) (immediate 698)))
       (id Andi))

      ((instruction "slli s5, s2, 8")
       (inputs ((instruction 00000000100010010001101010010011)))
       (outputs ((instruction 25) (rd 21) (rs1 18) (rs2 8) (immediate 8)))
       (id Slli))

      ((instruction "srli s5, s2, 21")
       (inputs ((instruction 00000001010110010101101010010011)))
       (outputs ((instruction 26) (rd 21) (rs1 18) (rs2 21) (immediate 21)))
       (id Srli))

      ((instruction "srai s5, s2, 17")
       (inputs ((instruction 01000001000110010101101010010011)))
       (outputs ((instruction 27) (rd 21) (rs1 18) (rs2 17) (immediate 17)))
       (id Srai))

      ((instruction "add s6, s5, s2")
       (inputs ((instruction 00000001001010101000101100110011)))
       (outputs ((instruction 28) (rd 22) (rs1 21) (rs2 18) (immediate 0)))
       (id Add))

      ((instruction "sub s6, s5, s2")
       (inputs ((instruction 01000001001010101000101100110011)))
       (outputs ((instruction 29) (rd 22) (rs1 21) (rs2 18) (immediate 0)))
       (id Sub))

      ((instruction "sll s6, s5, s2")
       (inputs ((instruction 00000001001010101001101100110011)))
       (outputs ((instruction 30) (rd 22) (rs1 21) (rs2 18) (immediate 0)))
       (id Sll))

      ((instruction "slt s6, s5, s2")
       (inputs ((instruction 00000001001010101010101100110011)))
       (outputs ((instruction 31) (rd 22) (rs1 21) (rs2 18) (immediate 0)))
       (id Slt))

      ((instruction "sltu s6, s5, s2")
       (inputs ((instruction 00000001001010101011101100110011)))
       (outputs ((instruction 32) (rd 22) (rs1 21) (rs2 18) (immediate 0)))
       (id Sltu))

      ((instruction "xor s6, s5, s2")
       (inputs ((instruction 00000001001010101100101100110011)))
       (outputs ((instruction 33) (rd 22) (rs1 21) (rs2 18) (immediate 0)))
       (id Xor))

      ((instruction "srl s6, s5, s2")
       (inputs ((instruction 00000001001010101101101100110011)))
       (outputs ((instruction 34) (rd 22) (rs1 21) (rs2 18) (immediate 0)))
       (id Srl))

      ((instruction "sra s6, s5, s2")
       (inputs ((instruction 01000001001010101101101100110011)))
       (outputs ((instruction 35) (rd 22) (rs1 21) (rs2 18) (immediate 0)))
       (id Sra))

      ((instruction "or s6, s5, s2")
       (inputs ((instruction 00000001001010101110101100110011)))
       (outputs ((instruction 36) (rd 22) (rs1 21) (rs2 18) (immediate 0)))
       (id Or))

      ((instruction "and s6, s5, s2")
       (inputs ((instruction 00000001001010101111101100110011)))
       (outputs ((instruction 37) (rd 22) (rs1 21) (rs2 18) (immediate 0)))
       (id And))

      ((instruction invalid)
       (inputs ((instruction 00000000000000000000000000000000)))
       (outputs ((instruction 0) (rd 0) (rs1 0) (rs2 0) (immediate 0)))
       (id Invalid)) |}]
  ;;
end
