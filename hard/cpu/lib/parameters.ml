open! Core

module Word_size : sig
  type t

  val of_bytes : int -> t
  val bits : t -> int
end = struct
  type t = int

  let of_bytes b =
    assert (b <> 0);
    b
  ;;

  let bits size = size * 8
end

let word_size = Word_size.of_bytes 4
let word_width = Word_size.bits word_size
let imem_size = Int.shift_left 1 18
let dmem_size = Int.shift_left 1 16
let stack_top = Int.shift_left 1 31
let code_bottom = Int.shift_left 1 20
let bootloader_start = Int.shift_left 1 14
let uart_io_address = Int.shift_left 1 13 + 3
let bootloader_bytes = [%blob "soft/onchip/boot.bin"]
