open! Core

let bytes = [%blob "soft/boot.bin"]

module For_testing = struct
  module Sample_programs = struct
    type t =
      | Invalid
      | Simple
      | String_search
      | Fibonacci
      | Atoi
      | Uart_echo
    [@@deriving sexp_of, compare, enumerate]
  end

  let sample = function
    | Sample_programs.Invalid -> List.init 8 ~f:Char.of_int_exn |> String.of_char_list
    | Simple -> [%blob "hard/cpu/test_binaries/simple.bin"]
    | String_search -> [%blob "hard/cpu/test_binaries/strchr.bin"]
    | Fibonacci -> [%blob "hard/cpu/test_binaries/fibonacci.bin"]
    | Atoi -> [%blob "hard/cpu/test_binaries/atoi.bin"]
    | Uart_echo -> [%blob "hard/cpu/test_binaries/uart_echo.bin"]
  ;;
end
