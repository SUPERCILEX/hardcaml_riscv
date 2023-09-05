open! Core
open Hardcaml

module Make
    (M : Interface.S)
    (Params : sig
       val address_bits : int
     end) =
struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; load : 'a
      ; read_address : 'a [@bits Params.address_bits]
      ; store : 'a
      ; write_address : 'a [@bits Params.address_bits]
      ; write_data : 'a M.t [@rtlprefix "wr$"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { data : 'a M.t [@rtlprefix "rd$"]
      ; hit : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create
    scope
    ~size
    ~address_to_index
    ~address_to_tag
    { I.clock
    ; load = read_enable
    ; read_address
    ; store = write_enable
    ; write_address
    ; write_data
    }
    =
    let open Signal in
    let module Line = struct
      type 'a t =
        { valid : 'a
        ; data : 'a M.t
        ; tag : 'a [@bits address_to_tag (zero Params.address_bits) |> width]
        }
      [@@deriving sexp_of, hardcaml]
    end
    in
    match
      Ram.create
        ~name:(Scope.name scope "mem")
        ~collision_mode:Read_before_write
        ~size
        ~write_ports:
          [| { write_clock = clock
             ; write_address =
                 (let address = address_to_index write_address in
                  assert (width address = address_bits_for size);
                  address)
             ; write_enable
             ; write_data =
                 Line.Of_signal.pack
                   { valid = vdd; data = write_data; tag = address_to_tag write_address }
             }
          |]
        ~read_ports:
          [| { read_clock = clock
             ; read_address = address_to_index read_address
             ; read_enable
             }
          |]
        ()
    with
    | [| data |] ->
      let { Line.valid; data; tag } = Line.Of_signal.unpack data in
      let tag_match =
        tag
        ==: (address_to_tag read_address
             |> reg ~enable:read_enable (Reg_spec.create ~clock ()))
      in
      { O.data; hit = valid &: tag_match }
    | _ -> failwith "Code out of date"
  ;;

  let hierarchical ~name ~size ~address_to_index ~address_to_tag scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name (create ~size ~address_to_index ~address_to_tag)
  ;;
end
