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
let imem_size = 1 lsl 16
let dmem_size = 1 lsl 15
let stack_top = 1 lsl 31
let code_bottom = 1 lsl 20
let bootloader_start = 1 lsl 14
let uart_io_address = (1 lsl 13) + 3
let bootloader_bytes = [%blob "soft/onchip/bootloader_server/boot.bin"]
