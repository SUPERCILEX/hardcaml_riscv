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
  let write_data_feedback = wire 8 in
  let write_ready_feedback = wire 1 in
  let read_ready_feedback = wire 1 in
  let { Uart_wrapper.O.transmit; write_done; read_data; read_done } =
    Uart_wrapper.circuit
      scope
      { Uart_wrapper.I.clock
      ; reset
      ; receive = uart_receive
      ; write_data = write_data_feedback
      ; write_ready = write_ready_feedback
      ; read_ready = read_ready_feedback
      }
  in
  let { Cpu.O.error; uart = { Cpu.Uart.O.write_data; write_ready; read_ready } } =
    Cpu.circuit
      scope
      { Cpu.I.clock; clear; uart = { Cpu.Uart.I.write_done; read_data; read_done } }
  in
  write_data_feedback <== write_data;
  write_ready_feedback <== write_ready;
  read_ready_feedback <== read_ready;
  { O.leds = error @: clear @: gnd @: gnd; uart_transmit = transmit }
;;
