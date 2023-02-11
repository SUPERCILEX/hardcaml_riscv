open! Core
open Hardcaml

let flip_flops ~clock value =
  let open Signal in
  pipeline
    ~attributes:[ Rtl_attribute.Vivado.async_reg true ]
    (Reg_spec.create ~clock ())
    value
;;
