open! Core

module I = struct
  type 'a t =
    { write_done : 'a
    ; read_data : 'a [@bits 8]
    ; read_ready : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { write_data : 'a [@bits 8]
    ; write_ready : 'a
    ; read_done : 'a
    }
  [@@deriving sexp_of, hardcaml]
end
