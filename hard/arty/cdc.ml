open! Core
open Hardcaml

let flip_flops ~clock =
  let open Signal in
  pipeline ~attributes:[ Rtl_attribute.Vivado.async_reg true ] (Reg_spec.create ~clock ())
;;
