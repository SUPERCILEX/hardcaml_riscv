open! Core
open Hardcaml

module I = struct
  type 'a t =
    { clock : 'a [@rtlname "CLK100MHZ"]
    ; switches : 'a [@bits 4] [@rtlname "sw"]
    ; buttons : 'a [@bits 4] [@rtlname "btn"]
    ; reset : 'a [@rtlname "ck_rst"]
    ; uart_receive : 'a [@rtlname "uart_txd_in"]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { leds : 'a [@bits 4] [@rtlname "led"]
    ; uart_transmit : 'a [@rtlname "uart_rxd_out"]
    }
  [@@deriving sexp_of, hardcaml]
end

let create
  (scope : Scope.t)
  ({ clock; switches = _; buttons = _; reset; uart_receive } : _ I.t)
  =
  let open Signal in
  let { Clk_wiz_0.O.locked
      ; clock_10_mhz = _
      ; clock_50_mhz = clock
      ; clock_100_mhz = _
      ; clock_166_mhz = _
      }
    =
    Clk_wiz_0.create { clock; reset }
  in
  let reset = Cdc.flip_flops ~clock ~n:2 locked in
  let clear = ~:reset in
  let ({ Cpu.Uart.O.write_data; write_ready; read_ready } as uart_feedback) =
    Cpu.Uart.O.Of_signal.wires ()
  in
  let { Uart_wrapper.O.transmit; write_done; read_data; read_done } =
    Uart_wrapper.circuit
      scope
      { Uart_wrapper.I.clock
      ; reset
      ; receive = uart_receive
      ; write_data
      ; write_ready
      ; read_ready
      }
  in
  let { Cpu.O.error; uart } =
    Cpu.circuit
      scope
      { Cpu.I.clock; clear; uart = { Cpu.Uart.I.write_done; read_data; read_done } }
  in
  Cpu.Uart.O.iter2 uart_feedback uart ~f:( <== );
  { O.leds = error @: gnd @: gnd @: gnd; uart_transmit = transmit }
;;
