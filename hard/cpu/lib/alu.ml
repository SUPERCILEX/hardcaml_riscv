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

let set_store instruction store =
  let open Signal in
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
  |> List.map ~f:(fun op -> Instruction.All.Rv32i op))
  @ ([ Instruction.RV32M.Mul; Mulh; Mulhsu; Mulhu; Div; Divu; Rem; Remu ]
    |> List.map ~f:(fun op -> Instruction.All.Rv32m op))
  |> List.map ~f:(fun op -> op, Always.[ store <-- vdd ])
  |> Instruction.Binary.Of_always.match_ ~default:[] instruction
;;

let create _scope { I.pc; instruction; rs1; rs2; immediate } =
  let open Signal in
  let ({ O.rd; store; jump; jump_target } as out) = O.Of_always.wire zero in
  Always.(
    compile
      [ set_store instruction store
      ; jump_target <-- pc +: immediate
      ; ([ Instruction.RV32I.Lui, [ rd <-- immediate ]
         ; Auipc, [ rd <-- pc +: immediate ]
         ; Jal, [ rd <-- pc +:. 4; jump <-- vdd ]
         ; ( Jalr
           , [ rd <-- pc +:. 4
             ; jump <-- vdd
             ; jump_target <-- concat_msb [ msbs (rs1 +: immediate); gnd ]
             ] )
         ; Beq, [ jump <-- (rs1 ==: rs2) ]
         ; Bne, [ jump <-- (rs1 <>: rs2) ]
         ; Blt, [ jump <-- (rs1 <+ rs2) ]
         ; Bge, [ jump <-- (rs1 >=+ rs2) ]
         ; Bltu, [ jump <-- (rs1 <: rs2) ]
         ; Bgeu, [ jump <-- (rs1 >=: rs2) ]
         ; Addi, [ rd <-- rs1 +: immediate ]
         ; Slti, [ rd <-- uresize (rs1 <+ immediate) 32 ]
         ; Sltiu, [ rd <-- uresize (rs1 <: immediate) 32 ]
         ; Xori, [ rd <-- rs1 ^: immediate ]
         ; Ori, [ rd <-- (rs1 |: immediate) ]
         ; Andi, [ rd <-- (rs1 &: immediate) ]
         ; Slli, [ rd <-- log_shift sll rs1 immediate ]
         ; Srli, [ rd <-- log_shift srl rs1 immediate ]
         ; Srai, [ rd <-- log_shift sra rs1 immediate ]
         ; Add, [ rd <-- rs1 +: rs2 ]
         ; Sub, [ rd <-- rs1 -: rs2 ]
         ; Sll, [ rd <-- log_shift sll rs1 (sel_bottom rs2 5) ]
         ; Slt, [ rd <-- uresize (rs1 <+ rs2) 32 ]
         ; Sltu, [ rd <-- uresize (rs1 <: rs2) 32 ]
         ; Xor, [ rd <-- rs1 ^: rs2 ]
         ; Srl, [ rd <-- log_shift srl rs1 (sel_bottom rs2 5) ]
         ; Sra, [ rd <-- log_shift sra rs1 (sel_bottom rs2 5) ]
         ; Or, [ rd <-- (rs1 |: rs2) ]
         ; And, [ rd <-- (rs1 &: rs2) ]
         ]
        |> List.map ~f:(Tuple2.map_fst ~f:(fun i -> Instruction.All.Rv32i i)))
        @ ([ Instruction.RV32M.Mul, [ rd <-- sel_bottom (rs1 *: rs2) 32 ]
           ; Mulh, [ rd <-- sel_top (rs1 *+ rs2) 32 ]
           ; Mulhu, [ rd <-- sel_top (rs1 *: rs2) 32 ]
           ; ( Mulhsu
             , [ rd <-- (se rs1 *+ ue rs2 |> Fn.flip drop_top 2 |> Fn.flip sel_top 32) ] )
           ]
          |> List.map ~f:(Tuple2.map_fst ~f:(fun i -> Instruction.All.Rv32m i)))
        |> Instruction.Binary.Of_always.match_ ~default:[] instruction
      ]);
  O.Of_always.value out
;;

let circuit scope =
  let module H = Hierarchy.In_scope (I) (O) in
  let module D = Debugging.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"alu" (D.create ~create_fn:create)
;;

module Tests = struct
  let test_bench (sim : (_ I.t, _ O.t) Cyclesim.t) =
    let open Bits in
    let inputs, outputs = Cyclesim.inputs sim, Cyclesim.outputs sim in
    let print_state () =
      let bits_and_int bits =
        [%message
          (bits : Bits.t) ~int:(to_int bits : int) ~signed_int:(to_sint bits : int)]
      in
      let instruction = Instruction.Binary.sim_get_exn inputs.instruction in
      let inputs = I.map inputs ~f:(( ! ) |> Fn.compose bits_and_int) in
      let outputs =
        let outputs = O.map outputs ~f:( ! ) in
        let for_bool o = to_bool o |> Bool.sexp_of_t in
        { O.rd = bits_and_int outputs.rd
        ; jump_target = bits_and_int outputs.jump_target
        ; store = for_bool outputs.store
        ; jump = for_bool outputs.jump
        }
      in
      Stdio.print_s
        [%message
          (instruction : Instruction.All.t) (inputs : Sexp.t I.t) (outputs : Sexp.t O.t)];
      Stdio.print_endline ""
    in
    let runi instruction ~rs1 ~rs2 ~immediate =
      Instruction.Binary.sim_set inputs.instruction instruction;
      inputs.rs1 := rs1;
      inputs.rs2 := rs2;
      inputs.immediate := immediate;
      Cyclesim.cycle sim;
      print_state ()
    in
    let run32i instruction ~rs1 ~rs2 ~immediate =
      runi (Rv32i instruction) ~rs1 ~rs2 ~immediate
    in
    let run32m instruction ~rs1 ~rs2 ~immediate =
      runi (Rv32m instruction) ~rs1 ~rs2 ~immediate
    in
    let bit_num = of_int ~width:Parameters.word_size in
    inputs.pc := bit_num 4206988;
    run32i Lui ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run32i Auipc ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run32i Jal ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run32i Jalr ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run32i Beq ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run32i Bne ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run32i Blt ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run32i Bge ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run32i Bltu ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run32i Bgeu ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run32i Lb ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run32i Lh ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run32i Lw ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run32i Lbu ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run32i Lhu ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run32i Sb ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run32i Sh ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run32i Sw ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run32i Addi ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run32i Slti ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run32i Sltiu ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run32i Xori ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run32i Ori ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run32i Andi ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run32i Slli ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run32i Srli ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run32i Srai ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run32i Add ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run32i Sub ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run32i Sll ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run32i Slt ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run32i Sltu ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run32i Xor ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run32i Srl ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run32i Sra ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run32i Or ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    run32i And ~rs1:(bit_num 69) ~rs2:(bit_num 42) ~immediate:(bit_num 88);
    List.cartesian_product [ 1; -1 ] [ 1; -1 ]
    |> List.map ~f:(fun (a, b) ->
         [ a * 1028091555 |> bit_num, b * 43 |> bit_num
         ; a * 42 |> bit_num, b * 69 |> bit_num
         ])
    |> List.concat
    |> List.iter ~f:(fun (rs1, rs2) ->
         run32m Mul ~rs1 ~rs2 ~immediate:(bit_num 88);
         run32m Mulh ~rs1 ~rs2 ~immediate:(bit_num 88);
         run32m Mulhsu ~rs1 ~rs2 ~immediate:(bit_num 88);
         run32m Mulhu ~rs1 ~rs2 ~immediate:(bit_num 88);
         run32m Div ~rs1 ~rs2 ~immediate:(bit_num 88);
         run32m Divu ~rs1 ~rs2 ~immediate:(bit_num 88);
         run32m Rem ~rs1 ~rs2 ~immediate:(bit_num 88);
         run32m Remu ~rs1 ~rs2 ~immediate:(bit_num 88))
  ;;

  let sim () =
    let module Simulator = Cyclesim.With_interface (I) (O) in
    let scope = Scope.create ~flatten_design:true () in
    let sim = Simulator.create ~config:Cyclesim.Config.trace_all (create scope) in
    test_bench sim
  ;;

  let%expect_test "Simple" =
    sim ();
    [%expect
      {|
      ((instruction (Rv32i Lui))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 000001) (int 1) (signed_int 1)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32i Auipc))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 000010) (int 2) (signed_int 2)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32i Jal))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 000011) (int 3) (signed_int 3)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd
          ((bits 00000000010000000011000110010000) (int 4206992)
           (signed_int 4206992)))
         (store true) (jump true)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32i Jalr))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 000100) (int 4) (signed_int 4)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd
          ((bits 00000000010000000011000110010000) (int 4206992)
           (signed_int 4206992)))
         (store true) (jump true)
         (jump_target
          ((bits 00000000000000000000000010011100) (int 156) (signed_int 156))))))

      ((instruction (Rv32i Beq))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 000101) (int 5) (signed_int 5)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store false) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32i Bne))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 000110) (int 6) (signed_int 6)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store false) (jump true)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32i Blt))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 000111) (int 7) (signed_int 7)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store false) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32i Bge))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 001000) (int 8) (signed_int 8)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store false) (jump true)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32i Bltu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 001001) (int 9) (signed_int 9)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store false) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32i Bgeu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 001010) (int 10) (signed_int 10)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store false) (jump true)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32i Lb))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 001011) (int 11) (signed_int 11)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32i Lh))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 001100) (int 12) (signed_int 12)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32i Lw))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 001101) (int 13) (signed_int 13)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32i Lbu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 001110) (int 14) (signed_int 14)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32i Lhu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 001111) (int 15) (signed_int 15)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32i Sb))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 010000) (int 16) (signed_int 16)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store false) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32i Sh))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 010001) (int 17) (signed_int 17)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store false) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32i Sw))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 010010) (int 18) (signed_int 18)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store false) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32i Addi))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 010011) (int 19) (signed_int 19)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000010011101) (int 157) (signed_int 157)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32i Slti))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 010100) (int 20) (signed_int 20)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000001) (int 1) (signed_int 1)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32i Sltiu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 010101) (int 21) (signed_int 21)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000001) (int 1) (signed_int 1)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32i Xori))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 010110) (int 22) (signed_int 22)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000011101) (int 29) (signed_int 29)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32i Ori))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 010111) (int 23) (signed_int 23)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000001011101) (int 93) (signed_int 93)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32i Andi))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 011000) (int 24) (signed_int 24)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000001000000) (int 64) (signed_int 64)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32i Slli))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 011001) (int 25) (signed_int 25)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32i Srli))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 011010) (int 26) (signed_int 26)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32i Srai))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 011011) (int 27) (signed_int 27)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32i Add))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 011100) (int 28) (signed_int 28)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000001101111) (int 111) (signed_int 111)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32i Sub))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 011101) (int 29) (signed_int 29)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000011011) (int 27) (signed_int 27)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32i Sll))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 011110) (int 30) (signed_int 30)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd
          ((bits 00000000000000010001010000000000) (int 70656) (signed_int 70656)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32i Slt))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 011111) (int 31) (signed_int 31)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32i Sltu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 100000) (int 32) (signed_int -32)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32i Xor))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 100001) (int 33) (signed_int -31)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000001101111) (int 111) (signed_int 111)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32i Srl))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 100010) (int 34) (signed_int -30)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32i Sra))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 100011) (int 35) (signed_int -29)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32i Or))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 100100) (int 36) (signed_int -28)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000001101111) (int 111) (signed_int 111)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32i And))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 100101) (int 37) (signed_int -27)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Mul))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 100110) (int 38) (signed_int -26)))
         (rs1
          ((bits 00111101010001110110111010100011) (int 1028091555)
           (signed_int 1028091555)))
         (rs2 ((bits 00000000000000000000000000101011) (int 43) (signed_int 43)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd
          ((bits 01001010111111111001010101100001) (int 1258263905)
           (signed_int 1258263905)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Mulh))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 100111) (int 39) (signed_int -25)))
         (rs1
          ((bits 00111101010001110110111010100011) (int 1028091555)
           (signed_int 1028091555)))
         (rs2 ((bits 00000000000000000000000000101011) (int 43) (signed_int 43)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000001010) (int 10) (signed_int 10)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Mulhsu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101000) (int 40) (signed_int -24)))
         (rs1
          ((bits 00111101010001110110111010100011) (int 1028091555)
           (signed_int 1028091555)))
         (rs2 ((bits 00000000000000000000000000101011) (int 43) (signed_int 43)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000001010) (int 10) (signed_int 10)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Mulhu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101001) (int 41) (signed_int -23)))
         (rs1
          ((bits 00111101010001110110111010100011) (int 1028091555)
           (signed_int 1028091555)))
         (rs2 ((bits 00000000000000000000000000101011) (int 43) (signed_int 43)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000001010) (int 10) (signed_int 10)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Div))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101010) (int 42) (signed_int -22)))
         (rs1
          ((bits 00111101010001110110111010100011) (int 1028091555)
           (signed_int 1028091555)))
         (rs2 ((bits 00000000000000000000000000101011) (int 43) (signed_int 43)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Divu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101011) (int 43) (signed_int -21)))
         (rs1
          ((bits 00111101010001110110111010100011) (int 1028091555)
           (signed_int 1028091555)))
         (rs2 ((bits 00000000000000000000000000101011) (int 43) (signed_int 43)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Rem))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101100) (int 44) (signed_int -20)))
         (rs1
          ((bits 00111101010001110110111010100011) (int 1028091555)
           (signed_int 1028091555)))
         (rs2 ((bits 00000000000000000000000000101011) (int 43) (signed_int 43)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Remu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101101) (int 45) (signed_int -19)))
         (rs1
          ((bits 00111101010001110110111010100011) (int 1028091555)
           (signed_int 1028091555)))
         (rs2 ((bits 00000000000000000000000000101011) (int 43) (signed_int 43)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Mul))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 100110) (int 38) (signed_int -26)))
         (rs1 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (rs2 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd
          ((bits 00000000000000000000101101010010) (int 2898) (signed_int 2898)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Mulh))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 100111) (int 39) (signed_int -25)))
         (rs1 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (rs2 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Mulhsu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101000) (int 40) (signed_int -24)))
         (rs1 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (rs2 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Mulhu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101001) (int 41) (signed_int -23)))
         (rs1 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (rs2 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Div))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101010) (int 42) (signed_int -22)))
         (rs1 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (rs2 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Divu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101011) (int 43) (signed_int -21)))
         (rs1 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (rs2 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Rem))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101100) (int 44) (signed_int -20)))
         (rs1 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (rs2 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Remu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101101) (int 45) (signed_int -19)))
         (rs1 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (rs2 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Mul))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 100110) (int 38) (signed_int -26)))
         (rs1
          ((bits 00111101010001110110111010100011) (int 1028091555)
           (signed_int 1028091555)))
         (rs2
          ((bits 11111111111111111111111111010101) (int 4294967253)
           (signed_int -43)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd
          ((bits 10110101000000000110101010011111) (int 3036703391)
           (signed_int -1258263905)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Mulh))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 100111) (int 39) (signed_int -25)))
         (rs1
          ((bits 00111101010001110110111010100011) (int 1028091555)
           (signed_int 1028091555)))
         (rs2
          ((bits 11111111111111111111111111010101) (int 4294967253)
           (signed_int -43)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd
          ((bits 11111111111111111111111111110101) (int 4294967285)
           (signed_int -11)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Mulhsu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101000) (int 40) (signed_int -24)))
         (rs1
          ((bits 00111101010001110110111010100011) (int 1028091555)
           (signed_int 1028091555)))
         (rs2
          ((bits 11111111111111111111111111010101) (int 4294967253)
           (signed_int -43)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd
          ((bits 00111101010001110110111010011000) (int 1028091544)
           (signed_int 1028091544)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Mulhu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101001) (int 41) (signed_int -23)))
         (rs1
          ((bits 00111101010001110110111010100011) (int 1028091555)
           (signed_int 1028091555)))
         (rs2
          ((bits 11111111111111111111111111010101) (int 4294967253)
           (signed_int -43)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd
          ((bits 00111101010001110110111010011000) (int 1028091544)
           (signed_int 1028091544)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Div))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101010) (int 42) (signed_int -22)))
         (rs1
          ((bits 00111101010001110110111010100011) (int 1028091555)
           (signed_int 1028091555)))
         (rs2
          ((bits 11111111111111111111111111010101) (int 4294967253)
           (signed_int -43)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Divu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101011) (int 43) (signed_int -21)))
         (rs1
          ((bits 00111101010001110110111010100011) (int 1028091555)
           (signed_int 1028091555)))
         (rs2
          ((bits 11111111111111111111111111010101) (int 4294967253)
           (signed_int -43)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Rem))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101100) (int 44) (signed_int -20)))
         (rs1
          ((bits 00111101010001110110111010100011) (int 1028091555)
           (signed_int 1028091555)))
         (rs2
          ((bits 11111111111111111111111111010101) (int 4294967253)
           (signed_int -43)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Remu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101101) (int 45) (signed_int -19)))
         (rs1
          ((bits 00111101010001110110111010100011) (int 1028091555)
           (signed_int 1028091555)))
         (rs2
          ((bits 11111111111111111111111111010101) (int 4294967253)
           (signed_int -43)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Mul))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 100110) (int 38) (signed_int -26)))
         (rs1 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (rs2
          ((bits 11111111111111111111111110111011) (int 4294967227)
           (signed_int -69)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd
          ((bits 11111111111111111111010010101110) (int 4294964398)
           (signed_int -2898)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Mulh))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 100111) (int 39) (signed_int -25)))
         (rs1 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (rs2
          ((bits 11111111111111111111111110111011) (int 4294967227)
           (signed_int -69)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd
          ((bits 11111111111111111111111111111111) (int 4294967295)
           (signed_int -1)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Mulhsu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101000) (int 40) (signed_int -24)))
         (rs1 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (rs2
          ((bits 11111111111111111111111110111011) (int 4294967227)
           (signed_int -69)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000101001) (int 41) (signed_int 41)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Mulhu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101001) (int 41) (signed_int -23)))
         (rs1 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (rs2
          ((bits 11111111111111111111111110111011) (int 4294967227)
           (signed_int -69)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000101001) (int 41) (signed_int 41)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Div))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101010) (int 42) (signed_int -22)))
         (rs1 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (rs2
          ((bits 11111111111111111111111110111011) (int 4294967227)
           (signed_int -69)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Divu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101011) (int 43) (signed_int -21)))
         (rs1 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (rs2
          ((bits 11111111111111111111111110111011) (int 4294967227)
           (signed_int -69)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Rem))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101100) (int 44) (signed_int -20)))
         (rs1 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (rs2
          ((bits 11111111111111111111111110111011) (int 4294967227)
           (signed_int -69)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Remu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101101) (int 45) (signed_int -19)))
         (rs1 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (rs2
          ((bits 11111111111111111111111110111011) (int 4294967227)
           (signed_int -69)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Mul))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 100110) (int 38) (signed_int -26)))
         (rs1
          ((bits 11000010101110001001000101011101) (int 3266875741)
           (signed_int -1028091555)))
         (rs2 ((bits 00000000000000000000000000101011) (int 43) (signed_int 43)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd
          ((bits 10110101000000000110101010011111) (int 3036703391)
           (signed_int -1258263905)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Mulh))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 100111) (int 39) (signed_int -25)))
         (rs1
          ((bits 11000010101110001001000101011101) (int 3266875741)
           (signed_int -1028091555)))
         (rs2 ((bits 00000000000000000000000000101011) (int 43) (signed_int 43)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd
          ((bits 11111111111111111111111111110101) (int 4294967285)
           (signed_int -11)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Mulhsu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101000) (int 40) (signed_int -24)))
         (rs1
          ((bits 11000010101110001001000101011101) (int 3266875741)
           (signed_int -1028091555)))
         (rs2 ((bits 00000000000000000000000000101011) (int 43) (signed_int 43)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd
          ((bits 11111111111111111111111111110101) (int 4294967285)
           (signed_int -11)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Mulhu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101001) (int 41) (signed_int -23)))
         (rs1
          ((bits 11000010101110001001000101011101) (int 3266875741)
           (signed_int -1028091555)))
         (rs2 ((bits 00000000000000000000000000101011) (int 43) (signed_int 43)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000100000) (int 32) (signed_int 32)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Div))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101010) (int 42) (signed_int -22)))
         (rs1
          ((bits 11000010101110001001000101011101) (int 3266875741)
           (signed_int -1028091555)))
         (rs2 ((bits 00000000000000000000000000101011) (int 43) (signed_int 43)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Divu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101011) (int 43) (signed_int -21)))
         (rs1
          ((bits 11000010101110001001000101011101) (int 3266875741)
           (signed_int -1028091555)))
         (rs2 ((bits 00000000000000000000000000101011) (int 43) (signed_int 43)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Rem))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101100) (int 44) (signed_int -20)))
         (rs1
          ((bits 11000010101110001001000101011101) (int 3266875741)
           (signed_int -1028091555)))
         (rs2 ((bits 00000000000000000000000000101011) (int 43) (signed_int 43)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Remu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101101) (int 45) (signed_int -19)))
         (rs1
          ((bits 11000010101110001001000101011101) (int 3266875741)
           (signed_int -1028091555)))
         (rs2 ((bits 00000000000000000000000000101011) (int 43) (signed_int 43)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Mul))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 100110) (int 38) (signed_int -26)))
         (rs1
          ((bits 11111111111111111111111111010110) (int 4294967254)
           (signed_int -42)))
         (rs2 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd
          ((bits 11111111111111111111010010101110) (int 4294964398)
           (signed_int -2898)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Mulh))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 100111) (int 39) (signed_int -25)))
         (rs1
          ((bits 11111111111111111111111111010110) (int 4294967254)
           (signed_int -42)))
         (rs2 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd
          ((bits 11111111111111111111111111111111) (int 4294967295)
           (signed_int -1)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Mulhsu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101000) (int 40) (signed_int -24)))
         (rs1
          ((bits 11111111111111111111111111010110) (int 4294967254)
           (signed_int -42)))
         (rs2 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd
          ((bits 11111111111111111111111111111111) (int 4294967295)
           (signed_int -1)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Mulhu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101001) (int 41) (signed_int -23)))
         (rs1
          ((bits 11111111111111111111111111010110) (int 4294967254)
           (signed_int -42)))
         (rs2 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000001000100) (int 68) (signed_int 68)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Div))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101010) (int 42) (signed_int -22)))
         (rs1
          ((bits 11111111111111111111111111010110) (int 4294967254)
           (signed_int -42)))
         (rs2 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Divu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101011) (int 43) (signed_int -21)))
         (rs1
          ((bits 11111111111111111111111111010110) (int 4294967254)
           (signed_int -42)))
         (rs2 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Rem))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101100) (int 44) (signed_int -20)))
         (rs1
          ((bits 11111111111111111111111111010110) (int 4294967254)
           (signed_int -42)))
         (rs2 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Remu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101101) (int 45) (signed_int -19)))
         (rs1
          ((bits 11111111111111111111111111010110) (int 4294967254)
           (signed_int -42)))
         (rs2 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Mul))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 100110) (int 38) (signed_int -26)))
         (rs1
          ((bits 11000010101110001001000101011101) (int 3266875741)
           (signed_int -1028091555)))
         (rs2
          ((bits 11111111111111111111111111010101) (int 4294967253)
           (signed_int -43)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd
          ((bits 01001010111111111001010101100001) (int 1258263905)
           (signed_int 1258263905)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Mulh))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 100111) (int 39) (signed_int -25)))
         (rs1
          ((bits 11000010101110001001000101011101) (int 3266875741)
           (signed_int -1028091555)))
         (rs2
          ((bits 11111111111111111111111111010101) (int 4294967253)
           (signed_int -43)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000001010) (int 10) (signed_int 10)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Mulhsu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101000) (int 40) (signed_int -24)))
         (rs1
          ((bits 11000010101110001001000101011101) (int 3266875741)
           (signed_int -1028091555)))
         (rs2
          ((bits 11111111111111111111111111010101) (int 4294967253)
           (signed_int -43)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd
          ((bits 11000010101110001001000101100111) (int 3266875751)
           (signed_int -1028091545)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Mulhu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101001) (int 41) (signed_int -23)))
         (rs1
          ((bits 11000010101110001001000101011101) (int 3266875741)
           (signed_int -1028091555)))
         (rs2
          ((bits 11111111111111111111111111010101) (int 4294967253)
           (signed_int -43)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd
          ((bits 11000010101110001001000100111100) (int 3266875708)
           (signed_int -1028091588)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Div))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101010) (int 42) (signed_int -22)))
         (rs1
          ((bits 11000010101110001001000101011101) (int 3266875741)
           (signed_int -1028091555)))
         (rs2
          ((bits 11111111111111111111111111010101) (int 4294967253)
           (signed_int -43)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Divu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101011) (int 43) (signed_int -21)))
         (rs1
          ((bits 11000010101110001001000101011101) (int 3266875741)
           (signed_int -1028091555)))
         (rs2
          ((bits 11111111111111111111111111010101) (int 4294967253)
           (signed_int -43)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Rem))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101100) (int 44) (signed_int -20)))
         (rs1
          ((bits 11000010101110001001000101011101) (int 3266875741)
           (signed_int -1028091555)))
         (rs2
          ((bits 11111111111111111111111111010101) (int 4294967253)
           (signed_int -43)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Remu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101101) (int 45) (signed_int -19)))
         (rs1
          ((bits 11000010101110001001000101011101) (int 3266875741)
           (signed_int -1028091555)))
         (rs2
          ((bits 11111111111111111111111111010101) (int 4294967253)
           (signed_int -43)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Mul))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 100110) (int 38) (signed_int -26)))
         (rs1
          ((bits 11111111111111111111111111010110) (int 4294967254)
           (signed_int -42)))
         (rs2
          ((bits 11111111111111111111111110111011) (int 4294967227)
           (signed_int -69)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd
          ((bits 00000000000000000000101101010010) (int 2898) (signed_int 2898)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Mulh))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 100111) (int 39) (signed_int -25)))
         (rs1
          ((bits 11111111111111111111111111010110) (int 4294967254)
           (signed_int -42)))
         (rs2
          ((bits 11111111111111111111111110111011) (int 4294967227)
           (signed_int -69)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Mulhsu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101000) (int 40) (signed_int -24)))
         (rs1
          ((bits 11111111111111111111111111010110) (int 4294967254)
           (signed_int -42)))
         (rs2
          ((bits 11111111111111111111111110111011) (int 4294967227)
           (signed_int -69)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd
          ((bits 11111111111111111111111111010110) (int 4294967254)
           (signed_int -42)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Mulhu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101001) (int 41) (signed_int -23)))
         (rs1
          ((bits 11111111111111111111111111010110) (int 4294967254)
           (signed_int -42)))
         (rs2
          ((bits 11111111111111111111111110111011) (int 4294967227)
           (signed_int -69)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd
          ((bits 11111111111111111111111110010001) (int 4294967185)
           (signed_int -111)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Div))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101010) (int 42) (signed_int -22)))
         (rs1
          ((bits 11111111111111111111111111010110) (int 4294967254)
           (signed_int -42)))
         (rs2
          ((bits 11111111111111111111111110111011) (int 4294967227)
           (signed_int -69)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Divu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101011) (int 43) (signed_int -21)))
         (rs1
          ((bits 11111111111111111111111111010110) (int 4294967254)
           (signed_int -42)))
         (rs2
          ((bits 11111111111111111111111110111011) (int 4294967227)
           (signed_int -69)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Rem))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101100) (int 44) (signed_int -20)))
         (rs1
          ((bits 11111111111111111111111111010110) (int 4294967254)
           (signed_int -42)))
         (rs2
          ((bits 11111111111111111111111110111011) (int 4294967227)
           (signed_int -69)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076))))))

      ((instruction (Rv32m Remu))
       (inputs
        ((pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101101) (int 45) (signed_int -19)))
         (rs1
          ((bits 11111111111111111111111111010110) (int 4294967254)
           (signed_int -42)))
         (rs2
          ((bits 11111111111111111111111110111011) (int 4294967227)
           (signed_int -69)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076)))))) |}]
  ;;
end
