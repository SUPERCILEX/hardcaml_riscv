open! Core
open Hardcaml

module Branch_target_buffer = struct
  module Entry = struct
    type 'a t = { taken_pc : 'a [@bits Parameters.word_width] }
    [@@deriving sexp_of, hardcaml]
  end

  include
    Cache.Make
      (Entry)
      (struct
        let address_bits = Parameters.word_width
      end)

  let hierarchical =
    let hash ~bits address =
      let open Signal in
      let random_prime =
        of_decimal_string
          ~width:1024
          "155932701214303184326864131316095681640290247695109515520724340505777567578309211013555223897242597556615485113571634396826737509550588918899636445222600177152071201620397262999574183535311513714762158246551801564970296048152341668550491010639120717043516543640455187677926056798474846669042658878200255193621"
      in
      let useful_address = msbs address in
      useful_address ^: sel_bottom random_prime (width useful_address)
      |> split_msb ~exact:false ~part_width:bits
      |> List.map ~f:(Fn.flip uresize bits)
      |> List.reduce_exn ~f:( ^: )
    in
    let size = 1024 in
    hierarchical
      ~name:"branch_target_buffer"
      ~size
      ~address_to_index:(hash ~bits:(Signal.address_bits_for size))
      ~address_to_tag:(hash ~bits:13)
  ;;
end
