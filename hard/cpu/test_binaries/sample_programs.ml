open! Core

type t =
  | Invalid
  | Simple
  | String_search
  | Fibonacci
  | Atoi
  | Uart_echo
  | Instruction_tests
  | Dependencies
  | Branch_prediction
[@@deriving sexp_of, compare, enumerate]

let program_bytes = function
  | Invalid -> List.init 8 ~f:Char.of_int_exn |> String.of_char_list
  | Simple -> [%blob "hard/cpu/test_binaries/simple.bin"]
  | String_search -> [%blob "hard/cpu/test_binaries/strchr.bin"]
  | Fibonacci -> [%blob "hard/cpu/test_binaries/fibonacci.bin"]
  | Atoi -> [%blob "hard/cpu/test_binaries/atoi.bin"]
  | Uart_echo -> [%blob "hard/cpu/test_binaries/uart_echo.bin"]
  | Instruction_tests -> [%blob "hard/cpu/test_binaries/instruction_tests.bin"]
  | Dependencies -> [%blob "hard/cpu/test_binaries/dependencies.bin"]
  | Branch_prediction -> [%blob "hard/cpu/test_binaries/branch_prediction.bin"]
;;
