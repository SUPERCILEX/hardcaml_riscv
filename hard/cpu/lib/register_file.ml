open! Core
open Hardcaml

module I = struct
  type 'a t =
    { clock : 'a
    ; load : 'a
    ; read_address1 : 'a [@bits 5]
    ; read_address2 : 'a [@bits 5]
    ; store : 'a
    ; write_address : 'a [@bits 5]
    ; write_data : 'a [@bits Parameters.word_width]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { rs1 : 'a [@bits Parameters.word_width]
    ; rs2 : 'a [@bits Parameters.word_width]
    }
  [@@deriving sexp_of, hardcaml]
end

let create
  scope
  { I.clock
  ; load = read_enable
  ; read_address1
  ; read_address2
  ; store = write_enable
  ; write_address
  ; write_data
  }
  =
  match
    Ram.create
      ~name:(Scope.name scope "mem")
      ~collision_mode:Read_before_write
      ~size:32
      ~write_ports:[| { write_clock = clock; write_address; write_enable; write_data } |]
      ~read_ports:
        [| { read_clock = clock; read_address = read_address1; read_enable }
         ; { read_clock = clock; read_address = read_address2; read_enable }
        |]
      ()
  with
  | [| rs1; rs2 |] -> { O.rs1; rs2 }
  | _ -> failwith "Code out of date"
;;

let hierarchical scope =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"register_file" create
;;
