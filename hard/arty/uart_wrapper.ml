open! Core
open Hardcaml

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; clock_100_mhz : 'a
    ; reset : 'a
    ; receive : 'a
    ; write_data : 'a [@bits 8]
    ; write_ready : 'a
    ; read_done : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { transmit : 'a
    ; write_done : 'a
    ; read_data : 'a [@bits 8]
    ; read_ready : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

let create
  (_scope : Scope.t)
  ({ clock; clear; clock_100_mhz; reset; receive; write_data; write_ready; read_done } :
    _ I.t)
  =
  let open Signal in
  let { Axi_uartlite_0.O.interrupt = _
      ; write_address_ready = _
      ; write_data_ready = _
      ; write_response
      ; write_response_valid
      ; read_address_ready = _
      ; read_data
      ; read_response
      ; read_response_valid
      ; transmit
      }
    =
    Axi_uartlite_0.create
      { Axi_uartlite_0.I.clock = clock_100_mhz
      ; reset
      ; write_address = of_hex ~width:4 "04"
      ; write_address_valid = vdd
      ; write_data = uresize write_data 32
      ; write_strobe = one 4
      ; write_data_valid = write_ready
      ; write_response_ready = vdd
      ; read_address = of_hex ~width:4 "00"
      ; read_address_valid = gnd
      ; read_response_ready = vdd
      ; receive
      }
    |> Axi_uartlite_0.O.map ~f:(Cdc.flip_flops ~clock ~n:2)
  in
  let read_data =
    reg_fb
      ~width:8
      ~f:(fun old -> mux2 read_done old (sel_bottom read_data 8))
      (Reg_spec.create ~clock ~clear ())
  in
  { O.transmit
  ; write_done = write_response_valid &: (write_response ==:. 0)
  ; read_data
  ; read_ready = read_response_valid &: (read_response ==:. 0)
  }
;;

let circuit scope =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"uart" create
;;
