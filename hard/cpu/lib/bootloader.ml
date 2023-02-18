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
    [@@deriving sexp_of, compare, enumerate]
  end

  let sample = function
    | Sample_programs.Invalid -> List.init 8 ~f:(Signal.of_int ~width:8)
    | Simple ->
      [%blob "cpu/test_binaries/simple.bin"]
      |> String.to_list
      |> List.map ~f:Signal.of_char
    | String_search ->
      [%blob "cpu/test_binaries/strchr.bin"]
      |> String.to_list
      |> List.map ~f:Signal.of_char
    | Fibonacci ->
      [%blob "cpu/test_binaries/fibonacci.bin"]
      |> String.to_list
      |> List.map ~f:Signal.of_char
  ;;
end
