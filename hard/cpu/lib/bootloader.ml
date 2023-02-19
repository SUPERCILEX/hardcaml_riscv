open! Core
open Hardcaml

(* TODO use real bootloader. *)
let bytes = List.init 8 ~f:(Signal.of_int ~width:8)

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

  let sample =
    let bin bytes = bytes |> String.to_list |> List.map ~f:Signal.of_char in
    function
    | Sample_programs.Invalid -> List.init 8 ~f:(Signal.of_int ~width:8)
    | Simple -> bin [%blob "cpu/test_binaries/simple.bin"]
    | String_search -> bin [%blob "cpu/test_binaries/strchr.bin"]
    | Fibonacci -> bin [%blob "cpu/test_binaries/fibonacci.bin"]
    | Atoi -> bin [%blob "cpu/test_binaries/atoi.bin"]
    | Uart_echo -> bin [%blob "cpu/test_binaries/uart_echo.bin"]
  ;;
end
