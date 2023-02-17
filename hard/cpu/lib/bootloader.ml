open! Core
open Hardcaml

(* TODO use real bootloader. *)
let bytes = List.init 8 ~f:(Signal.of_int ~width:8)

module For_testing = struct
  module Sample_programs = struct
    type t = Invalid [@@deriving sexp_of, compare, enumerate]
  end

  let sample = function
    | Sample_programs.Invalid -> List.init 8 ~f:(Signal.of_int ~width:8)
  ;;
end
