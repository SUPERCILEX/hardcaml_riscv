open! Core
open Hardcaml

module I = struct
  type 'a t =
    { clock : 'a
    ; resetn : 'a
    ; receive : 'a
    ; uart : 'a Cpu.Uart.O.t
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { transmit : 'a
    ; uart : 'a Cpu.Uart.I.t
    }
  [@@deriving sexp_of, hardcaml]
end

let create
  _scope
  { I.clock; resetn; receive; uart = { write_data; write_ready; read_ready } }
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
      { Axi_uartlite_0.I.clock
      ; resetn
      ; write_address = of_hex ~width:4 "04"
      ; write_address_valid = write_ready
      ; write_data = uresize write_data 32
      ; write_strobe = one 4
      ; write_data_valid = write_ready
      ; write_response_ready = vdd
      ; read_address = of_hex ~width:4 "00"
      ; read_address_valid = read_ready &: ~:write_ready
      ; read_response_ready = vdd
      ; receive
      }
  in
  { O.transmit
  ; uart =
      { write_done = write_response_valid &: (write_response ==:. 0)
      ; read_data = sel_bottom read_data 8
      ; read_done = read_response_valid &: (read_response ==:. 0)
      }
  }
;;

let hierarchical scope =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"uart" create
;;
