open! Core
open Hardcaml

module RV32I = struct
  type t =
    | Invalid (* Not a real instruction, indicates a decode error *)
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

module RV32M = struct
  type t =
    | Mul (* Multiply *)
    | Mulh (* Multiply high *)
    | Mulhsu (* Multiply high signed unsigned *)
    | Mulhu (* Multiply high unsigned *)
    | Div (* Divide *)
    | Divu (* Divide unsigned *)
    | Rem (* Remainder *)
    | Remu (* Remainder unsigned *)
  [@@deriving sexp_of, compare, enumerate]
end

module All = struct
  type t =
    | Rv32i of RV32I.t
    | Rv32m of RV32M.t
  [@@deriving sexp_of, compare, enumerate]
end

include Enum.Make_enums (All)
