open! Core
open Hardcaml

module I = struct
  type 'a t =
    { pc : 'a [@bits Parameters.word_size]
    ; instruction : 'a Instruction.Binary.t
    ; rs1 : 'a [@bits Parameters.word_size]
    ; rs2 : 'a [@bits Parameters.word_size]
    ; immediate : 'a [@bits 32]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { rd : 'a [@bits Parameters.word_size]
    ; store : 'a (* Whether or not to store the result back into the register file. *)
    ; jump : 'a
    ; jump_target : 'a [@bits Parameters.word_size]
    }
  [@@deriving sexp_of, hardcaml]
end

let create (scope : Scope.t) (i : _ I.t) =
  let open Signal in
  let rd = Always.Variable.wire ~default:(zero Parameters.word_size) in
  let store = Always.Variable.wire ~default:gnd in
  let jump = Always.Variable.wire ~default:gnd in
  let jump_target = Always.Variable.wire ~default:(zero Parameters.word_size) in
  let _debugging =
    let ( -- ) = Scope.naming scope in
    let _ = rd.value -- "result" in
    let _ = store.value -- "store_to_reg_file" in
    let _ = jump.value -- "should_jump" in
    let _ = jump_target.value -- "jump_target" in
    ()
  in
  Always.(
    compile
      [ Instruction.Binary.Of_always.match_
          ~default:[]
          i.instruction
          (Instruction.RV32I.
             [ Lui
             ; Auipc
             ; Jal
             ; Jalr
             ; Lb
             ; Lh
             ; Lw
             ; Lbu
             ; Lhu
             ; Addi
             ; Slti
             ; Sltiu
             ; Xori
             ; Ori
             ; Andi
             ; Slli
             ; Srli
             ; Srai
             ; Add
             ; Sub
             ; Sll
             ; Slt
             ; Sltu
             ; Xor
             ; Srl
             ; Sra
             ; Or
             ; And
             ]
          |> List.map ~f:(fun op -> op, [ store <-- vdd ]))
      ; Instruction.Binary.Of_always.match_
          ~default:[]
          i.instruction
          [ Lui, [ rd <-- i.immediate ]
          ; Auipc, [ rd <-- i.pc +: i.immediate ]
          ; Jal, [ rd <-- i.pc +:. 4; jump <-- vdd; jump_target <-- i.pc +: i.immediate ]
          ; ( Jalr
            , [ rd <-- i.pc +:. 4
              ; jump <-- vdd
              ; jump_target <-- concat_msb [ msbs (i.rs1 +: i.immediate); gnd ]
              ] )
          ; Beq, [ jump <-- (i.rs1 ==: i.rs2); jump_target <-- i.pc +: i.immediate ]
          ; Bne, [ jump <-- (i.rs1 <>: i.rs2); jump_target <-- i.pc +: i.immediate ]
          ; Blt, [ jump <-- (i.rs1 <: i.rs2); jump_target <-- i.pc +: i.immediate ]
          ; Bge, [ jump <-- (i.rs1 >=: i.rs2); jump_target <-- i.pc +: i.immediate ]
          ; Bltu, [ jump <-- (i.rs1 <+ i.rs2); jump_target <-- i.pc +: i.immediate ]
          ; Bgeu, [ jump <-- (i.rs1 >=+ i.rs2); jump_target <-- i.pc +: i.immediate ]
          ; Lb, [ rd <-- sresize (sel_bottom i.rs1 8) 32 ]
          ; Lh, [ rd <-- sresize (sel_bottom i.rs1 16) 32 ]
          ; Lw, [ rd <-- i.rs1 ]
          ; Lbu, [ rd <-- uresize (sel_bottom i.rs1 8) 32 ]
          ; Lhu, [ rd <-- uresize (sel_bottom i.rs1 16) 32 ]
          ; Addi, [ rd <-- i.rs1 +: i.immediate ]
          ; Slti, [ rd <-- uresize (i.rs1 <: i.immediate) 32 ]
          ; Sltiu, [ rd <-- uresize (i.rs1 <+ i.immediate) 32 ]
          ; Xori, [ rd <-- i.rs1 ^: i.immediate ]
          ; Ori, [ rd <-- (i.rs1 |: i.immediate) ]
          ; Andi, [ rd <-- (i.rs1 &: i.immediate) ]
          ; Slli, [ rd <-- Alu.shift_mux ~f:sll i.rs1 i.immediate ]
          ; Srli, [ rd <-- Alu.shift_mux ~f:srl i.rs1 i.immediate ]
          ; Srai, [ rd <-- Alu.shift_mux ~f:sra i.rs1 i.immediate ]
          ; Add, [ rd <-- i.rs1 +: i.rs2 ]
          ; Sub, [ rd <-- i.rs1 -: i.rs2 ]
          ; Sll, [ rd <-- Alu.shift_mux ~f:sll i.rs1 (sel_bottom i.rs2 5) ]
          ; Slt, [ rd <-- uresize (i.rs1 <: i.rs2) 32 ]
          ; Sltu, [ rd <-- uresize (i.rs1 <+ i.rs2) 32 ]
          ; Xor, [ rd <-- i.rs1 ^: i.rs2 ]
          ; Srl, [ rd <-- Alu.shift_mux ~f:srl i.rs1 (sel_bottom i.rs2 5) ]
          ; Sra, [ rd <-- Alu.shift_mux ~f:sra i.rs1 (sel_bottom i.rs2 5) ]
          ; Or, [ rd <-- (i.rs1 |: i.rs2) ]
          ; And, [ rd <-- (i.rs1 &: i.rs2) ]
          ]
      ]);
  { O.rd = rd.value
  ; store = store.value
  ; jump = jump.value
  ; jump_target = jump_target.value
  }
;;

let circuit scope =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"alu_control" create
;;

module Tests = struct
  module Simulator = Cyclesim.With_interface (I) (O)
  module Waveform = Hardcaml_waveterm.Waveform

  let test_bench (sim : (_ I.t, _ O.t) Cyclesim.t) =
    let inputs, outputs = Cyclesim.inputs sim, Cyclesim.outputs sim in
    let print_state () =
      let instruction = Instruction.Binary.sim_get_exn inputs.instruction in
      let pc_int = Bits.to_int !(inputs.pc) in
      let rs1_int = Bits.to_int !(inputs.rs1) in
      let rs2_int = Bits.to_int !(inputs.rs2) in
      let immediate_int = Bits.to_int !(inputs.immediate) in
      let pc_bits = !(inputs.pc) in
      let rs1_bits = !(inputs.rs1) in
      let rs2_bits = !(inputs.rs2) in
      let immediate_bits = !(inputs.immediate) in
      let store = Bits.to_bool !(outputs.store) in
      let jump = Bits.to_bool !(outputs.jump) in
      let rd_int = Bits.to_int !(outputs.rd) in
      let jump_target_int = Bits.to_int !(outputs.jump_target) in
      let rd_bits = !(outputs.rd) in
      let jump_target_bits = !(outputs.jump_target) in
      Stdio.print_s
        [%message
          (instruction : Instruction.RV32I.t)
            (pc_int : int)
            (rs1_int : int)
            (rs2_int : int)
            (immediate_int : int)
            (pc_bits : Bits.t)
            (rs1_bits : Bits.t)
            (rs2_bits : Bits.t)
            (immediate_bits : Bits.t)
            (store : bool)
            (jump : bool)
            (rd_int : int)
            (jump_target_int : int)
            (rd_bits : Bits.t)
            (jump_target_bits : Bits.t)];
      Stdio.print_endline ""
    in
    let run instruction ~rs1 ~rs2 ~immediate =
      Instruction.Binary.sim_set inputs.instruction instruction;
      inputs.rs1 := rs1;
      inputs.rs2 := rs2;
      inputs.immediate := immediate;
      Cyclesim.cycle sim;
      print_state ()
    in
    let bit_num = Bits.of_int ~width:Parameters.word_size in
    inputs.pc := bit_num 4206988;
    run Lui ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run Auipc ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run Jal ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run Jalr ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run Beq ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run Bne ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run Blt ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run Bge ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run Bltu ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run Bgeu ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run Lb ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run Lh ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run Lw ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run Lbu ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run Lhu ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run Sb ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run Sh ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run Sw ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run Addi ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run Slti ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run Sltiu ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run Xori ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run Ori ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run Andi ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run Slli ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run Srli ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run Srai ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run Add ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run Sub ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run Sll ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run Slt ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run Sltu ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run Xor ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run Srl ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run Sra ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run Or ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run And ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88)
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
      ((instruction Lui) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store true) (jump false)
       (rd_int 88) (jump_target_int 0) (rd_bits 00000000000000000000000001011000)
       (jump_target_bits 00000000000000000000000000000000))

      ((instruction Auipc) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store true) (jump false)
       (rd_int 4207076) (jump_target_int 0)
       (rd_bits 00000000010000000011000111100100)
       (jump_target_bits 00000000000000000000000000000000))

      ((instruction Jal) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store true) (jump true)
       (rd_int 4206992) (jump_target_int 4207076)
       (rd_bits 00000000010000000011000110010000)
       (jump_target_bits 00000000010000000011000111100100))

      ((instruction Jalr) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store true) (jump true)
       (rd_int 4206992) (jump_target_int 156)
       (rd_bits 00000000010000000011000110010000)
       (jump_target_bits 00000000000000000000000010011100))

      ((instruction Beq) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store false) (jump false)
       (rd_int 0) (jump_target_int 4207076)
       (rd_bits 00000000000000000000000000000000)
       (jump_target_bits 00000000010000000011000111100100))

      ((instruction Bne) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store false) (jump true)
       (rd_int 0) (jump_target_int 4207076)
       (rd_bits 00000000000000000000000000000000)
       (jump_target_bits 00000000010000000011000111100100))

      ((instruction Blt) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store false) (jump false)
       (rd_int 0) (jump_target_int 4207076)
       (rd_bits 00000000000000000000000000000000)
       (jump_target_bits 00000000010000000011000111100100))

      ((instruction Bge) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store false) (jump true)
       (rd_int 0) (jump_target_int 4207076)
       (rd_bits 00000000000000000000000000000000)
       (jump_target_bits 00000000010000000011000111100100))

      ((instruction Bltu) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store false) (jump false)
       (rd_int 0) (jump_target_int 4207076)
       (rd_bits 00000000000000000000000000000000)
       (jump_target_bits 00000000010000000011000111100100))

      ((instruction Bgeu) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store false) (jump true)
       (rd_int 0) (jump_target_int 4207076)
       (rd_bits 00000000000000000000000000000000)
       (jump_target_bits 00000000010000000011000111100100))

      ((instruction Lb) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store true) (jump false)
       (rd_int 69) (jump_target_int 0) (rd_bits 00000000000000000000000001000101)
       (jump_target_bits 00000000000000000000000000000000))

      ((instruction Lh) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store true) (jump false)
       (rd_int 69) (jump_target_int 0) (rd_bits 00000000000000000000000001000101)
       (jump_target_bits 00000000000000000000000000000000))

      ((instruction Lw) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store true) (jump false)
       (rd_int 69) (jump_target_int 0) (rd_bits 00000000000000000000000001000101)
       (jump_target_bits 00000000000000000000000000000000))

      ((instruction Lbu) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store true) (jump false)
       (rd_int 69) (jump_target_int 0) (rd_bits 00000000000000000000000001000101)
       (jump_target_bits 00000000000000000000000000000000))

      ((instruction Lhu) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store true) (jump false)
       (rd_int 69) (jump_target_int 0) (rd_bits 00000000000000000000000001000101)
       (jump_target_bits 00000000000000000000000000000000))

      ((instruction Sb) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store false) (jump false)
       (rd_int 0) (jump_target_int 0) (rd_bits 00000000000000000000000000000000)
       (jump_target_bits 00000000000000000000000000000000))

      ((instruction Sh) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store false) (jump false)
       (rd_int 0) (jump_target_int 0) (rd_bits 00000000000000000000000000000000)
       (jump_target_bits 00000000000000000000000000000000))

      ((instruction Sw) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store false) (jump false)
       (rd_int 0) (jump_target_int 0) (rd_bits 00000000000000000000000000000000)
       (jump_target_bits 00000000000000000000000000000000))

      ((instruction Addi) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store true) (jump false)
       (rd_int 157) (jump_target_int 0) (rd_bits 00000000000000000000000010011101)
       (jump_target_bits 00000000000000000000000000000000))

      ((instruction Slti) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store true) (jump false)
       (rd_int 1) (jump_target_int 0) (rd_bits 00000000000000000000000000000001)
       (jump_target_bits 00000000000000000000000000000000))

      ((instruction Sltiu) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store true) (jump false)
       (rd_int 1) (jump_target_int 0) (rd_bits 00000000000000000000000000000001)
       (jump_target_bits 00000000000000000000000000000000))

      ((instruction Xori) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store true) (jump false)
       (rd_int 29) (jump_target_int 0) (rd_bits 00000000000000000000000000011101)
       (jump_target_bits 00000000000000000000000000000000))

      ((instruction Ori) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store true) (jump false)
       (rd_int 93) (jump_target_int 0) (rd_bits 00000000000000000000000001011101)
       (jump_target_bits 00000000000000000000000000000000))

      ((instruction Andi) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store true) (jump false)
       (rd_int 64) (jump_target_int 0) (rd_bits 00000000000000000000000001000000)
       (jump_target_bits 00000000000000000000000000000000))

      ((instruction Slli) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store true) (jump false)
       (rd_int 1157627904) (jump_target_int 0)
       (rd_bits 01000101000000000000000000000000)
       (jump_target_bits 00000000000000000000000000000000))

      ((instruction Srli) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store true) (jump false)
       (rd_int 0) (jump_target_int 0) (rd_bits 00000000000000000000000000000000)
       (jump_target_bits 00000000000000000000000000000000))

      ((instruction Srai) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store true) (jump false)
       (rd_int 0) (jump_target_int 0) (rd_bits 00000000000000000000000000000000)
       (jump_target_bits 00000000000000000000000000000000))

      ((instruction Add) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store true) (jump false)
       (rd_int 111) (jump_target_int 0) (rd_bits 00000000000000000000000001101111)
       (jump_target_bits 00000000000000000000000000000000))

      ((instruction Sub) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store true) (jump false)
       (rd_int 27) (jump_target_int 0) (rd_bits 00000000000000000000000000011011)
       (jump_target_bits 00000000000000000000000000000000))

      ((instruction Sll) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store true) (jump false)
       (rd_int 70656) (jump_target_int 0)
       (rd_bits 00000000000000010001010000000000)
       (jump_target_bits 00000000000000000000000000000000))

      ((instruction Slt) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store true) (jump false)
       (rd_int 0) (jump_target_int 0) (rd_bits 00000000000000000000000000000000)
       (jump_target_bits 00000000000000000000000000000000))

      ((instruction Sltu) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store true) (jump false)
       (rd_int 0) (jump_target_int 0) (rd_bits 00000000000000000000000000000000)
       (jump_target_bits 00000000000000000000000000000000))

      ((instruction Xor) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store true) (jump false)
       (rd_int 111) (jump_target_int 0) (rd_bits 00000000000000000000000001101111)
       (jump_target_bits 00000000000000000000000000000000))

      ((instruction Srl) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store true) (jump false)
       (rd_int 0) (jump_target_int 0) (rd_bits 00000000000000000000000000000000)
       (jump_target_bits 00000000000000000000000000000000))

      ((instruction Sra) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store true) (jump false)
       (rd_int 0) (jump_target_int 0) (rd_bits 00000000000000000000000000000000)
       (jump_target_bits 00000000000000000000000000000000))

      ((instruction Or) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store true) (jump false)
       (rd_int 111) (jump_target_int 0) (rd_bits 00000000000000000000000001101111)
       (jump_target_bits 00000000000000000000000000000000))

      ((instruction And) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store true) (jump false)
       (rd_int 0) (jump_target_int 0) (rd_bits 00000000000000000000000000000000)
       (jump_target_bits 00000000000000000000000000000000)) |}]
  ;;
end
