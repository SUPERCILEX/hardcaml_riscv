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

let shift_mux ~f a shift =
  let open Signal in
  let w = width a in
  mux (sel_bottom shift (address_bits_for w)) (List.init w ~f:(fun shift -> f a shift))
;;

let create _scope { I.pc; data; instruction; rs1; rs2; immediate } =
  let open Signal in
  let ({ O.rd; store; jump; jump_target } as out) = O.Of_always.wire zero in
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
      ; rd <-- data
      ; jump_target <-- pc +: immediate
      ; Instruction.Binary.Of_always.match_
          ~default:[]
          instruction
          [ Lui, [ rd <-- immediate ]
          ; Auipc, [ rd <-- pc +: immediate ]
          ; Jal, [ rd <-- pc +:. 4; jump <-- vdd ]
          ; ( Jalr
            , [ rd <-- pc +:. 4
              ; jump <-- vdd
              ; jump_target <-- concat_msb [ msbs (rs1 +: immediate); gnd ]
              ] )
          ; Beq, [ jump <-- (rs1 ==: rs2) ]
          ; Bne, [ jump <-- (rs1 <>: rs2) ]
          ; Blt, [ jump <-- (rs1 <: rs2) ]
          ; Bge, [ jump <-- (rs1 >=: rs2) ]
          ; Bltu, [ jump <-- (rs1 <+ rs2) ]
          ; Bgeu, [ jump <-- (rs1 >=+ rs2) ]
          ; Sb, [ rd <-- rs2 ]
          ; Sh, [ rd <-- rs2 ]
          ; Sw, [ rd <-- rs2 ]
          ; Addi, [ rd <-- rs1 +: immediate ]
          ; Slti, [ rd <-- uresize (rs1 <: immediate) 32 ]
          ; Sltiu, [ rd <-- uresize (rs1 <+ immediate) 32 ]
          ; Xori, [ rd <-- rs1 ^: immediate ]
          ; Ori, [ rd <-- (rs1 |: immediate) ]
          ; Andi, [ rd <-- (rs1 &: immediate) ]
          ; Slli, [ rd <-- shift_mux ~f:sll rs1 immediate ]
          ; Srli, [ rd <-- shift_mux ~f:srl rs1 immediate ]
          ; Srai, [ rd <-- shift_mux ~f:sra rs1 immediate ]
          ; Add, [ rd <-- rs1 +: rs2 ]
          ; Sub, [ rd <-- rs1 -: rs2 ]
          ; Sll, [ rd <-- shift_mux ~f:sll rs1 (sel_bottom rs2 5) ]
          ; Slt, [ rd <-- uresize (rs1 <: rs2) 32 ]
          ; Sltu, [ rd <-- uresize (rs1 <+ rs2) 32 ]
          ; Xor, [ rd <-- rs1 ^: rs2 ]
          ; Srl, [ rd <-- shift_mux ~f:srl rs1 (sel_bottom rs2 5) ]
          ; Sra, [ rd <-- shift_mux ~f:sra rs1 (sel_bottom rs2 5) ]
          ; Or, [ rd <-- (rs1 |: rs2) ]
          ; And, [ rd <-- (rs1 &: rs2) ]
          ]
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
        let int = to_int bits in
        [%message (bits : Bits.t) (int : int)]
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
          (instruction : Instruction.RV32I.t) (inputs : Sexp.t I.t) (outputs : Sexp.t O.t)];
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
    let module Simulator = Cyclesim.With_interface (I) (O) in
    let scope = Scope.create ~flatten_design:true () in
    let sim = Simulator.create ~config:Cyclesim.Config.trace_all (create scope) in
    test_bench sim
  ;;

  let%expect_test "Simple" =
    sim ();
    [%expect
      {|
      ((instruction Lui)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 000001) (int 1)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000001011000) (int 88))) (store true)
         (jump false)
         (jump_target ((bits 00000000010000000011000111100100) (int 4207076))))))

      ((instruction Auipc)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 000010) (int 2)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 00000000010000000011000111100100) (int 4207076))) (store true)
         (jump false)
         (jump_target ((bits 00000000010000000011000111100100) (int 4207076))))))

      ((instruction Jal)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 000011) (int 3)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 00000000010000000011000110010000) (int 4206992))) (store true)
         (jump true)
         (jump_target ((bits 00000000010000000011000111100100) (int 4207076))))))

      ((instruction Jalr)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 000100) (int 4)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 00000000010000000011000110010000) (int 4206992))) (store true)
         (jump true)
         (jump_target ((bits 00000000000000000000000010011100) (int 156))))))

      ((instruction Beq)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 000101) (int 5)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000001000101) (int 69))) (store false)
         (jump false)
         (jump_target ((bits 00000000010000000011000111100100) (int 4207076))))))

      ((instruction Bne)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 000110) (int 6)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000001000101) (int 69))) (store false)
         (jump true)
         (jump_target ((bits 00000000010000000011000111100100) (int 4207076))))))

      ((instruction Blt)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 000111) (int 7)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000001000101) (int 69))) (store false)
         (jump false)
         (jump_target ((bits 00000000010000000011000111100100) (int 4207076))))))

      ((instruction Bge)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 001000) (int 8)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000001000101) (int 69))) (store false)
         (jump true)
         (jump_target ((bits 00000000010000000011000111100100) (int 4207076))))))

      ((instruction Bltu)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 001001) (int 9)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000001000101) (int 69))) (store false)
         (jump false)
         (jump_target ((bits 00000000010000000011000111100100) (int 4207076))))))

      ((instruction Bgeu)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 001010) (int 10)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000001000101) (int 69))) (store false)
         (jump true)
         (jump_target ((bits 00000000010000000011000111100100) (int 4207076))))))

      ((instruction Lb)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 001011) (int 11)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000001000101) (int 69))) (store true)
         (jump false)
         (jump_target ((bits 00000000010000000011000111100100) (int 4207076))))))

      ((instruction Lh)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 001100) (int 12)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000001000101) (int 69))) (store true)
         (jump false)
         (jump_target ((bits 00000000010000000011000111100100) (int 4207076))))))

      ((instruction Lw)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 001101) (int 13)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000001000101) (int 69))) (store true)
         (jump false)
         (jump_target ((bits 00000000010000000011000111100100) (int 4207076))))))

      ((instruction Lbu)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 001110) (int 14)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000001000101) (int 69))) (store true)
         (jump false)
         (jump_target ((bits 00000000010000000011000111100100) (int 4207076))))))

      ((instruction Lhu)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 001111) (int 15)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000001000101) (int 69))) (store true)
         (jump false)
         (jump_target ((bits 00000000010000000011000111100100) (int 4207076))))))

      ((instruction Sb)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 010000) (int 16)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000101010) (int 42))) (store false)
         (jump false)
         (jump_target ((bits 00000000010000000011000111100100) (int 4207076))))))

      ((instruction Sh)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 010001) (int 17)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000101010) (int 42))) (store false)
         (jump false)
         (jump_target ((bits 00000000010000000011000111100100) (int 4207076))))))

      ((instruction Sw)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 010010) (int 18)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000101010) (int 42))) (store false)
         (jump false)
         (jump_target ((bits 00000000010000000011000111100100) (int 4207076))))))

      ((instruction Addi)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 010011) (int 19)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000010011101) (int 157))) (store true)
         (jump false)
         (jump_target ((bits 00000000010000000011000111100100) (int 4207076))))))

      ((instruction Slti)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 010100) (int 20)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000001) (int 1))) (store true)
         (jump false)
         (jump_target ((bits 00000000010000000011000111100100) (int 4207076))))))

      ((instruction Sltiu)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 010101) (int 21)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000001) (int 1))) (store true)
         (jump false)
         (jump_target ((bits 00000000010000000011000111100100) (int 4207076))))))

      ((instruction Xori)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 010110) (int 22)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000011101) (int 29))) (store true)
         (jump false)
         (jump_target ((bits 00000000010000000011000111100100) (int 4207076))))))

      ((instruction Ori)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 010111) (int 23)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000001011101) (int 93))) (store true)
         (jump false)
         (jump_target ((bits 00000000010000000011000111100100) (int 4207076))))))

      ((instruction Andi)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 011000) (int 24)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000001000000) (int 64))) (store true)
         (jump false)
         (jump_target ((bits 00000000010000000011000111100100) (int 4207076))))))

      ((instruction Slli)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 011001) (int 25)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 01000101000000000000000000000000) (int 1157627904)))
         (store true) (jump false)
         (jump_target ((bits 00000000010000000011000111100100) (int 4207076))))))

      ((instruction Srli)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 011010) (int 26)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0))) (store true)
         (jump false)
         (jump_target ((bits 00000000010000000011000111100100) (int 4207076))))))

      ((instruction Srai)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 011011) (int 27)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0))) (store true)
         (jump false)
         (jump_target ((bits 00000000010000000011000111100100) (int 4207076))))))

      ((instruction Add)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 011100) (int 28)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000001101111) (int 111))) (store true)
         (jump false)
         (jump_target ((bits 00000000010000000011000111100100) (int 4207076))))))

      ((instruction Sub)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 011101) (int 29)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000011011) (int 27))) (store true)
         (jump false)
         (jump_target ((bits 00000000010000000011000111100100) (int 4207076))))))

      ((instruction Sll)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 011110) (int 30)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 00000000000000010001010000000000) (int 70656))) (store true)
         (jump false)
         (jump_target ((bits 00000000010000000011000111100100) (int 4207076))))))

      ((instruction Slt)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 011111) (int 31)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0))) (store true)
         (jump false)
         (jump_target ((bits 00000000010000000011000111100100) (int 4207076))))))

      ((instruction Sltu)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 100000) (int 32)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0))) (store true)
         (jump false)
         (jump_target ((bits 00000000010000000011000111100100) (int 4207076))))))

      ((instruction Xor)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 100001) (int 33)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000001101111) (int 111))) (store true)
         (jump false)
         (jump_target ((bits 00000000010000000011000111100100) (int 4207076))))))

      ((instruction Srl)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 100010) (int 34)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0))) (store true)
         (jump false)
         (jump_target ((bits 00000000010000000011000111100100) (int 4207076))))))

      ((instruction Sra)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 100011) (int 35)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0))) (store true)
         (jump false)
         (jump_target ((bits 00000000010000000011000111100100) (int 4207076))))))

      ((instruction Or)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 100100) (int 36)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000001101111) (int 111))) (store true)
         (jump false)
         (jump_target ((bits 00000000010000000011000111100100) (int 4207076))))))

      ((instruction And)
       (inputs
        ((pc ((bits 00000000010000000011000110001100) (int 4206988)))
         (data ((bits 00000000000000000000000001000101) (int 69)))
         (instruction ((bits 100101) (int 37)))
         (rs1 ((bits 00000000000000000000000001000101) (int 69)))
         (rs2 ((bits 00000000000000000000000000101010) (int 42)))
         (immediate ((bits 00000000000000000000000001011000) (int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0))) (store true)
         (jump false)
         (jump_target ((bits 00000000010000000011000111100100) (int 4207076)))))) |}]
  ;;
end
