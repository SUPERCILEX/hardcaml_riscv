open! Core
open Hardcaml

module I = struct
  type 'a t =
    { clock : 'a [@rtlname "CLK100MHZ"]
    ; switches : 'a [@bits 4] [@rtlname "sw"]
    ; buttons : 'a [@bits 4] [@rtlname "btn"]
    ; reset : 'a [@rtlname "ck_rst"]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { leds : 'a [@bits 4] [@rtlname "led"] } [@@deriving sexp_of, hardcaml]
end

let cdc_trigger ~clock value =
  let open Signal in
  pipeline
    ~attributes:[ Rtl_attribute.Vivado.async_reg true ]
    (Reg_spec.create ~clock ())
    value
;;

let create (scope : Scope.t) ({ clock; switches = _; buttons = _; reset } : _ I.t) =
  let open Signal in
  let { Clocking_wizard.O.locked
      ; clock_10_mhz = _
      ; clock_25_mhz = _
      ; clock_50_mhz = clock
      ; clock_100_mhz = _
      ; clock_166_mhz = _
      ; clock_200_mhz = _
      }
    =
    Clocking_wizard.create { clock; reset }
  in
  let { Cpu.O._unused } =
    Cpu.circuit scope { Cpu.I.clock; clear = ~:(cdc_trigger ~clock ~n:2 locked) }
  in
  { O.leds = uresize _unused 4 }
;;
