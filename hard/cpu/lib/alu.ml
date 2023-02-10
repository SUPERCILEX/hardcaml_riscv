open! Core
open Hardcaml

module I = struct
  type 'a t =
    { pc : 'a [@bits Parameters.word_size]
    ; data : 'a [@bits Parameters.word_size]
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

let create (scope : Scope.t) ({ pc; data; instruction; rs1; rs2; immediate } : _ I.t) =
  let open Signal in
  let rd = Always.Variable.wire ~default:(zero Parameters.word_size) in
  let store = Always.Variable.wire ~default:gnd in
  let jump = Always.Variable.wire ~default:gnd in
  let jump_target = Always.Variable.wire ~default:(zero Parameters.word_size) in
  let _debugging =
    let ( -- ) = Scope.naming scope in
    ignore (rd.value -- "result");
    ignore (store.value -- "store_to_reg_file");
    ignore (jump.value -- "should_jump");
    ignore (jump_target.value -- "jump_target")
  in
  Always.(
    compile
      [ Instruction.Binary.Of_always.match_
          ~default:[]
          instruction
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
          instruction
          [ Lui, [ rd <-- immediate ]
          ; Auipc, [ rd <-- pc +: immediate ]
          ; Jal, [ rd <-- pc +:. 4; jump <-- vdd; jump_target <-- pc +: immediate ]
          ; ( Jalr
            , [ rd <-- pc +:. 4
              ; jump <-- vdd
              ; jump_target <-- concat_msb [ msbs (rs1 +: immediate); gnd ]
              ] )
          ; Beq, [ jump <-- (rs1 ==: rs2); jump_target <-- pc +: immediate ]
          ; Bne, [ jump <-- (rs1 <>: rs2); jump_target <-- pc +: immediate ]
          ; Blt, [ jump <-- (rs1 <: rs2); jump_target <-- pc +: immediate ]
          ; Bge, [ jump <-- (rs1 >=: rs2); jump_target <-- pc +: immediate ]
          ; Bltu, [ jump <-- (rs1 <+ rs2); jump_target <-- pc +: immediate ]
          ; Bgeu, [ jump <-- (rs1 >=+ rs2); jump_target <-- pc +: immediate ]
          ; Lb, [ rd <-- sresize (sel_bottom data 8) 32 ]
          ; Lh, [ rd <-- sresize (sel_bottom data 16) 32 ]
          ; Lw, [ rd <-- data ]
          ; Lbu, [ rd <-- uresize (sel_bottom data 8) 32 ]
          ; Lhu, [ rd <-- uresize (sel_bottom data 16) 32 ]
          ; Sb, [ rd <-- uresize (sel_bottom rs2 8) 32 ]
          ; Sh, [ rd <-- uresize (sel_bottom rs2 16) 32 ]
          ; Sw, [ rd <-- rs2 ]
          ; Addi, [ rd <-- rs1 +: immediate ]
          ; Slti, [ rd <-- uresize (rs1 <: immediate) 32 ]
          ; Sltiu, [ rd <-- uresize (rs1 <+ immediate) 32 ]
          ; Xori, [ rd <-- rs1 ^: immediate ]
          ; Ori, [ rd <-- (rs1 |: immediate) ]
          ; Andi, [ rd <-- (rs1 &: immediate) ]
          ; Slli, [ rd <-- Alu_utils.shift_mux ~f:sll rs1 immediate ]
          ; Srli, [ rd <-- Alu_utils.shift_mux ~f:srl rs1 immediate ]
          ; Srai, [ rd <-- Alu_utils.shift_mux ~f:sra rs1 immediate ]
          ; Add, [ rd <-- rs1 +: rs2 ]
          ; Sub, [ rd <-- rs1 -: rs2 ]
          ; Sll, [ rd <-- Alu_utils.shift_mux ~f:sll rs1 (sel_bottom rs2 5) ]
          ; Slt, [ rd <-- uresize (rs1 <: rs2) 32 ]
          ; Sltu, [ rd <-- uresize (rs1 <+ rs2) 32 ]
          ; Xor, [ rd <-- rs1 ^: rs2 ]
          ; Srl, [ rd <-- Alu_utils.shift_mux ~f:srl rs1 (sel_bottom rs2 5) ]
          ; Sra, [ rd <-- Alu_utils.shift_mux ~f:sra rs1 (sel_bottom rs2 5) ]
          ; Or, [ rd <-- (rs1 |: rs2) ]
          ; And, [ rd <-- (rs1 &: rs2) ]
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
    let open Bits in
    let inputs, outputs = Cyclesim.inputs sim, Cyclesim.outputs sim in
    let print_state () =
      let instruction = Instruction.Binary.sim_get_exn inputs.instruction in
      let pc_int = to_int !(inputs.pc) in
      let rs1_int = to_int !(inputs.rs1) in
      let rs2_int = to_int !(inputs.rs2) in
      let immediate_int = to_int !(inputs.immediate) in
      let pc_bits = !(inputs.pc) in
      let rs1_bits = !(inputs.rs1) in
      let rs2_bits = !(inputs.rs2) in
      let immediate_bits = !(inputs.immediate) in
      let store = to_bool !(outputs.store) in
      let jump = to_bool !(outputs.jump) in
      let rd_int = to_int !(outputs.rd) in
      let jump_target_int = to_int !(outputs.jump_target) in
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
      inputs.data := rs1;
      Cyclesim.cycle sim;
      print_state ()
    in
    let bit_num = of_int ~width:Parameters.word_size in
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
       (rd_int 42) (jump_target_int 0) (rd_bits 00000000000000000000000000101010)
       (jump_target_bits 00000000000000000000000000000000))

      ((instruction Sh) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store false) (jump false)
       (rd_int 42) (jump_target_int 0) (rd_bits 00000000000000000000000000101010)
       (jump_target_bits 00000000000000000000000000000000))

      ((instruction Sw) (pc_int 4206988) (rs1_int 69) (rs2_int 42)
       (immediate_int 88) (pc_bits 00000000010000000011000110001100)
       (rs1_bits 00000000000000000000000001000101)
       (rs2_bits 00000000000000000000000000101010)
       (immediate_bits 00000000000000000000000001011000) (store false) (jump false)
       (rd_int 42) (jump_target_int 0) (rd_bits 00000000000000000000000000101010)
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
