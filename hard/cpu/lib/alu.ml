open! Core
open Hardcaml

module I = struct
  type 'a t =
    { clock : 'a
    ; reset : 'a
    ; active : 'a
    ; pc : 'a [@bits Parameters.word_size]
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
    ; stall : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module Make_divider (Params : sig
  val bits : int
end) =
struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; reset : 'a
      ; start : 'a
      ; dividend : 'a [@bits Params.bits]
      ; divisor : 'a [@bits Params.bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { quotient : 'a [@bits Params.bits]
      ; remainder : 'a [@bits Params.bits]
      ; ready : 'a
      ; error : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create scope { I.clock; reset; start; dividend; divisor } =
    let open Signal in
    let spec = Reg_spec.create ~clock ~reset ~clear:start () in
    let ( -- ) = Scope.naming scope in
    let qr =
      let width = 2 * Params.bits in
      Always.Variable.reg
        ~width
        (spec |> Reg_spec.override ~clear_to:(uresize dividend width))
    in
    qr.value -- "qr" |> ignore;
    let bit =
      let width = address_bits_for Params.bits + 1 in
      reg_fb
        ~width
        ~f:(fun bit -> mux2 (bit ==:. 0) bit (bit -:. 1))
        (spec |> Reg_spec.override ~clear_to:(of_int ~width Params.bits))
      -- "bit"
    in
    let diff = sel_top qr.value (Params.bits + 1) -: ue divisor -- "diff" in
    Always.(
      compile
        [ if_
            (diff <+. 0)
            [ qr <-- sll qr.value 1 ]
            [ qr <-- lsbs diff @: sel_bottom qr.value (Params.bits - 1) @: vdd ]
        ]);
    let div_by_zero = divisor ==:. 0 in
    let error = div_by_zero in
    { O.quotient = mux2 div_by_zero (ones Params.bits) (sel_bottom qr.value Params.bits)
    ; remainder = mux2 div_by_zero dividend (sel_top qr.value Params.bits)
    ; error
    ; ready = bit ==:. 0 |: error
    }
  ;;

  let circuit scope =
    let module H = Hierarchy.In_scope (I) (O) in
    let module D = Debugging.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"divider" (D.create ~create_fn:create)
  ;;
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

let create scope { I.clock; reset; active; pc; instruction; rs1; rs2; immediate } =
  let open Signal in
  let ({ O.rd; store; jump; jump_target; stall } as out) = O.Of_always.wire zero in
  let module Divider =
    Make_divider (struct
      let bits = Parameters.word_size
    end)
  in
  let start_divider = Always.Variable.wire ~default:gnd in
  let dividend = Always.Variable.wire ~default:rs1 in
  let divisor = Always.Variable.wire ~default:rs2 in
  let divided =
    Divider.circuit
      scope
      { Divider.I.clock
      ; reset
      ; start = start_divider.value
      ; dividend = dividend.value
      ; divisor = divisor.value
      }
  in
  Always.(
    compile
      [ set_store instruction store
      ; jump_target <-- pc +: immediate
      ; [ Instruction.RV32I.Lui, [ rd <-- immediate ]
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
        |> List.map ~f:(Tuple2.map_fst ~f:(fun i -> Instruction.All.Rv32i i))
        |> Instruction.Binary.Of_always.match_ ~default:[] instruction
      ; [ Instruction.RV32M.Mul, [ rd <-- sel_bottom (rs1 *: rs2) 32 ]
        ; Mulh, [ rd <-- sel_top (rs1 *+ rs2) 32 ]
        ; Mulhu, [ rd <-- sel_top (rs1 *: rs2) 32 ]
        ; ( Mulhsu
          , [ rd <-- (se rs1 *+ ue rs2 |> Fn.flip drop_top 2 |> Fn.flip sel_top 32) ] )
        ]
        |> List.map ~f:(Tuple2.map_fst ~f:(fun i -> Instruction.All.Rv32m i))
        |> Instruction.Binary.Of_always.match_ ~default:[] instruction
      ; (let { Divider.O.quotient; remainder; ready; error } = divided in
         let debounce p =
           let p_delayed = reg (Reg_spec.create ~clock ~reset ()) p in
           p &: ~:p_delayed
         in
         let abs p = mux2 (p <+. 0) (negate p) p in
         let is_dividing = Variable.wire ~default:gnd in
         let was_dividing = is_dividing.value |> reg (Reg_spec.create ~clock ~reset ()) in
         [ ( Instruction.RV32M.Div
           , [ rd <-- mux2 (msb rs1 <>: msb rs2) (negate quotient) quotient
             ; dividend <-- abs rs1
             ; divisor <-- abs rs2
             ] )
         ; Instruction.RV32M.Divu, [ rd <-- quotient ]
         ; ( Instruction.RV32M.Rem
           , [ rd <-- mux2 (rs1 <+. 0) (negate remainder) remainder
             ; dividend <-- abs rs1
             ; divisor <-- abs rs2
             ] )
         ; Instruction.RV32M.Remu, [ rd <-- remainder ]
         ]
         |> List.map ~f:(fun (i, statements) ->
              ( Instruction.All.Rv32m i
              , statements
                @ [ is_dividing <-- active
                  ; start_divider <-- (ready |: ~:was_dividing)
                  ; stall <-- (~:ready |: debounce is_dividing.value &: ~:error &: active)
                  ] ))
         |> Instruction.Binary.Of_always.match_ ~default:[] instruction)
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
        ; stall = for_bool outputs.stall
        }
      in
      Stdio.print_s
        [%message
          (instruction : Instruction.All.t) (inputs : Sexp.t I.t) (outputs : Sexp.t O.t)];
      Stdio.print_endline "";
      ()
    in
    let bit_num = of_int ~width:Parameters.word_size in
    let runi
      instruction
      ?(rs1 = bit_num 69)
      ?(rs2 = bit_num 42)
      ?(immediate = bit_num 88)
      ()
      =
      Instruction.Binary.sim_set inputs.instruction instruction;
      inputs.rs1 := rs1;
      inputs.rs2 := rs2;
      inputs.immediate := immediate;
      Cyclesim.cycle sim;
      while to_bool !(outputs.stall) do
        Cyclesim.cycle sim
      done;
      print_state ();
      ()
    in
    let run32i instruction ?rs1 ?rs2 ?immediate =
      runi (Rv32i instruction) ?rs1 ?rs2 ?immediate
    in
    let run32m instruction ?rs1 ?rs2 ?immediate =
      runi (Rv32m instruction) ?rs1 ?rs2 ?immediate
    in
    inputs.pc := bit_num 4206988;
    inputs.active := vdd;
    run32i Lui ();
    run32i Auipc ();
    run32i Jal ();
    run32i Jalr ();
    run32i Beq ();
    run32i Bne ();
    run32i Blt ();
    run32i Bge ();
    run32i Bltu ();
    run32i Bgeu ();
    run32i Lb ();
    run32i Lh ();
    run32i Lw ();
    run32i Lbu ();
    run32i Lhu ();
    run32i Sb ();
    run32i Sh ();
    run32i Sw ();
    run32i Addi ();
    run32i Slti ();
    run32i Sltiu ();
    run32i Xori ();
    run32i Ori ();
    run32i Andi ();
    run32i Slli ();
    run32i Srli ();
    run32i Srai ();
    run32i Add ();
    run32i Sub ();
    run32i Sll ();
    run32i Slt ();
    run32i Sltu ();
    run32i Xor ();
    run32i Srl ();
    run32i Sra ();
    run32i Or ();
    run32i And ();
    let _m =
      let check_division_equation ?quotient ?remainder convert =
        let dividend = convert !(inputs.rs1) in
        let divisor = convert !(inputs.rs2) in
        let quotient = Option.value quotient ~default:(dividend / divisor) in
        let remainder = Option.value remainder ~default:(dividend mod divisor) in
        if not (dividend = (divisor * quotient) + remainder)
        then
          raise_s
            [%message (dividend : int) (divisor : int) (quotient : int) (remainder : int)]
      in
      let check_division ~quotient convert =
        check_division_equation ~quotient convert;
        let dividend = convert !(inputs.rs1) in
        let divisor = convert !(inputs.rs2) in
        if not (quotient = dividend / divisor)
        then raise_s [%message (dividend : int) (divisor : int) (quotient : int)]
      in
      let check_remainder ~remainder convert =
        check_division_equation ~remainder convert;
        let dividend = convert !(inputs.rs1) in
        let divisor = convert !(inputs.rs2) in
        if not (remainder = dividend mod divisor)
        then raise_s [%message (dividend : int) (divisor : int) (remainder : int)]
      in
      List.cartesian_product [ 1; -1 ] [ 1; -1 ]
      |> List.map ~f:(fun (a, b) ->
           [ a * 1028091555 |> bit_num, b * 43 |> bit_num
           ; a * 42 |> bit_num, b * 69 |> bit_num
           ])
      |> List.concat
      |> List.iter ~f:(fun (rs1, rs2) ->
           run32m Mul ~rs1 ~rs2 ();
           run32m Mulh ~rs1 ~rs2 ();
           run32m Mulhsu ~rs1 ~rs2 ();
           run32m Mulhu ~rs1 ~rs2 ();
           run32m Div ~rs1 ~rs2 ();
           check_division ~quotient:(to_sint !(outputs.rd)) to_sint;
           run32m Divu ~rs1 ~rs2 ();
           check_division ~quotient:(to_int !(outputs.rd)) to_int;
           run32m Rem ~rs1 ~rs2 ();
           check_remainder ~remainder:(to_sint !(outputs.rd)) to_sint;
           run32m Remu ~rs1 ~rs2 ();
           check_remainder ~remainder:(to_int !(outputs.rd)) to_int;
           ());
      let _overflow =
        let rs1 = sll (one Parameters.word_size) (Parameters.word_size - 1) in
        let rs2 = ones Parameters.word_size in
        run32m Div ~rs1 ~rs2 ();
        run32m Rem ~rs1 ~rs2 ();
        ()
      in
      ()
    in
    ()
  ;;

  let sim () =
    let module Simulator = Cyclesim.With_interface (I) (O) in
    let scope = Scope.create ~flatten_design:true () in
    let sim = Simulator.create ~config:Cyclesim.Config.trace_all (create scope) in
    test_bench sim;
    ()
  ;;

  let%expect_test "Simple" =
    sim ();
    [%expect
      {|
      ((instruction (Rv32i Lui))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32i Auipc))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32i Jal))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32i Jalr))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
          ((bits 00000000000000000000000010011100) (int 156) (signed_int 156)))
         (stall false))))

      ((instruction (Rv32i Beq))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32i Bne))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32i Blt))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32i Bge))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32i Bltu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32i Bgeu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32i Lb))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32i Lh))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32i Lw))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32i Lbu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32i Lhu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32i Sb))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32i Sh))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32i Sw))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32i Addi))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32i Slti))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32i Sltiu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32i Xori))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32i Ori))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32i Andi))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32i Slli))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32i Srli))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32i Srai))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32i Add))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32i Sub))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32i Sll))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32i Slt))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32i Sltu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32i Xor))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32i Srl))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32i Sra))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32i Or))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32i And))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Mul))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Mulh))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Mulhsu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Mulhu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Div))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
        ((rd
          ((bits 00000001011011001101001011110001) (int 23909105)
           (signed_int 23909105)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Divu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
        ((rd
          ((bits 00000001011011001101001011110001) (int 23909105)
           (signed_int 23909105)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Rem))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
        ((rd ((bits 00000000000000000000000000101000) (int 40) (signed_int 40)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Remu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
        ((rd ((bits 00000000000000000000000000101000) (int 40) (signed_int 40)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Mul))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Mulh))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Mulhsu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Mulhu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Div))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Divu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Rem))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101100) (int 44) (signed_int -20)))
         (rs1 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (rs2 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Remu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101101) (int 45) (signed_int -19)))
         (rs1 ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (rs2 ((bits 00000000000000000000000001000101) (int 69) (signed_int 69)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Mul))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Mulh))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Mulhsu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Mulhu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Div))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
        ((rd
          ((bits 11111110100100110010110100001111) (int 4271058191)
           (signed_int -23909105)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Divu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Rem))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
        ((rd ((bits 00000000000000000000000000101000) (int 40) (signed_int 40)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Remu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
        ((rd
          ((bits 00111101010001110110111010100011) (int 1028091555)
           (signed_int 1028091555)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Mul))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Mulh))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Mulhsu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Mulhu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Div))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Divu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Rem))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
        ((rd ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Remu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
        ((rd ((bits 00000000000000000000000000101010) (int 42) (signed_int 42)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Mul))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Mulh))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Mulhsu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Mulhu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Div))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
        ((rd
          ((bits 11111110100100110010110100001111) (int 4271058191)
           (signed_int -23909105)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Divu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
        ((rd
          ((bits 00000100100001110100010011011110) (int 75973854)
           (signed_int 75973854)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Rem))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
        ((rd
          ((bits 11111111111111111111111111011000) (int 4294967256)
           (signed_int -40)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Remu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
        ((rd ((bits 00000000000000000000000000010011) (int 19) (signed_int 19)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Mul))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Mulh))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Mulhsu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Mulhu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Div))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Divu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
        ((rd
          ((bits 00000011101101011100110000001110) (int 62245902)
           (signed_int 62245902)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Rem))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
        ((rd
          ((bits 11111111111111111111111111010110) (int 4294967254)
           (signed_int -42)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Remu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
        ((rd ((bits 00000000000000000000000000010000) (int 16) (signed_int 16)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Mul))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Mulh))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Mulhsu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Mulhu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Div))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
        ((rd
          ((bits 00000001011011001101001011110001) (int 23909105)
           (signed_int 23909105)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Divu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Rem))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
        ((rd
          ((bits 11111111111111111111111111011000) (int 4294967256)
           (signed_int -40)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Remu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
        ((rd
          ((bits 11000010101110001001000101011101) (int 3266875741)
           (signed_int -1028091555)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Mul))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Mulh))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Mulhsu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Mulhu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Div))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Divu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
        ((rd ((bits 00000000000000000000000000000001) (int 1) (signed_int 1)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Rem))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
        ((rd
          ((bits 11111111111111111111111111010110) (int 4294967254)
           (signed_int -42)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Remu))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
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
        ((rd ((bits 00000000000000000000000000011011) (int 27) (signed_int 27)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Div))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101010) (int 42) (signed_int -22)))
         (rs1
          ((bits 10000000000000000000000000000000) (int 2147483648)
           (signed_int -2147483648)))
         (rs2
          ((bits 11111111111111111111111111111111) (int 4294967295)
           (signed_int -1)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd
          ((bits 10000000000000000000000000000000) (int 2147483648)
           (signed_int -2147483648)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076)))
         (stall false))))

      ((instruction (Rv32m Rem))
       (inputs
        ((clock ((bits 0) (int 0) (signed_int 0)))
         (reset ((bits 0) (int 0) (signed_int 0)))
         (active ((bits 1) (int 1) (signed_int -1)))
         (pc
          ((bits 00000000010000000011000110001100) (int 4206988)
           (signed_int 4206988)))
         (instruction ((bits 101100) (int 44) (signed_int -20)))
         (rs1
          ((bits 10000000000000000000000000000000) (int 2147483648)
           (signed_int -2147483648)))
         (rs2
          ((bits 11111111111111111111111111111111) (int 4294967295)
           (signed_int -1)))
         (immediate
          ((bits 00000000000000000000000001011000) (int 88) (signed_int 88)))))
       (outputs
        ((rd ((bits 00000000000000000000000000000000) (int 0) (signed_int 0)))
         (store true) (jump false)
         (jump_target
          ((bits 00000000010000000011000111100100) (int 4207076)
           (signed_int 4207076)))
         (stall false)))) |}]
  ;;
end
