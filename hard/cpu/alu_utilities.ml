open! Core
open Hardcaml

let shift_mux ~f a shift =
  let open Signal in
  let w = width a in
  mux (sel_bottom shift (address_bits_for w)) (List.init w ~f:(fun shift -> f a shift))
;;

let flip_endianness d =
  let open Signal in
  let bits = width d in
  assert (bits % 8 = 0);
  List.init (bits / 8) ~f:(fun i -> d.:[((i + 1) * 8) - 1, i * 8]) |> concat_msb
;;
