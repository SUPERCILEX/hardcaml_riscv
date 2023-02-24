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

let create scope { I.clock; switches = _; buttons = _; reset; uart_receive } =
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
  let resetn = Cdc.flip_flops ~clock ~n:2 locked in
  let reset = ~:reset in
  let uart_feedback = Cpu.Uart.O.Of_signal.wires () in
  let { Uart_wrapper.O.transmit; uart } =
    Uart_wrapper.circuit
      scope
      { Uart_wrapper.I.clock; resetn; receive = uart_receive; uart = uart_feedback }
  in
  let { Cpu.O.error; uart; cycles_since_boot = _ } =
    Cpu.circuit scope { Cpu.I.clock; reset; uart }
  in
  Cpu.Uart.O.iter2 uart_feedback uart ~f:( <== );
  { O.leds = error @: gnd @: gnd @: gnd; uart_transmit = transmit }
;;
