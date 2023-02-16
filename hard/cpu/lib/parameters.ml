open! Core

let word_size = 32
let imem_size = Int.shift_left 1 16
let dmem_size = Int.shift_left 1 16
let stack_top = Int.shift_left 1 31
let code_bottom = Int.shift_left 1 20
let () = assert (word_size % 8 = 0)
let boot_rom_start = Int.shift_left 1 14
