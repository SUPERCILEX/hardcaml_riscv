open! Core
open Hardcaml

let shift_mux ~f a shift =
  let open Signal in
  let w = width a in
  mux (sel_bottom shift (address_bits_for w)) (List.init w ~f:(fun shift -> f a shift))
;;
