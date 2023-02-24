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
  let open Signal in
  let ( -- ) = Scope.naming scope in
  match
    Ram.create
      ~name:"register_file"
      ~collision_mode:Read_before_write
      ~size:32
      ~write_ports:
        [| { Ram.Write_port.write_clock = clock
           ; write_address
           ; write_enable = (write_enable &: (write_address <>:. 0)) -- "write_enable"
           ; write_data
           }
        |]
      ~read_ports:
        [| { Ram.Read_port.read_clock = clock; read_address = read_address1; read_enable }
         ; { Ram.Read_port.read_clock = clock; read_address = read_address2; read_enable }
        |]
      ()
  with
  | [| rs1; rs2 |] -> { O.rs1; rs2 }
  | _ -> assert false
;;

let circuit scope =
  let module H = Hierarchy.In_scope (I) (O) in
  let module D = Debugging.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"register_file" (D.create ~create_fn:create)
;;
