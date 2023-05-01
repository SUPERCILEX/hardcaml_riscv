open! Core
open Hardcaml

module Make
  (M : Hardcaml.Interface.S) (Params : sig
    val size : int
    val address_bits : int
    val address_to_index : Signal.t -> Signal.t
    val address_to_tag : Signal.t -> Signal.t
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
    ~name
    scope
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
        ; tag : 'a [@bits Params.(address_to_tag (zero address_bits) |> width)]
        }
      [@@deriving sexp_of, hardcaml]
    end
    in
    match
      Ram.create
        ~name:(Scope.name scope name)
        ~collision_mode:Read_before_write
        ~size:Params.size
        ~write_ports:
          [| { Ram.Write_port.write_clock = clock
             ; write_address =
                 (let address = Params.address_to_index write_address in
                  assert (width address = address_bits_for Params.size);
                  address)
             ; write_enable
             ; write_data =
                 Line.Of_signal.pack
                   { Line.valid = vdd
                   ; data = write_data
                   ; tag = Params.address_to_tag write_address
                   }
             }
          |]
        ~read_ports:
          [| { Ram.Read_port.read_clock = clock
             ; read_address = Params.address_to_index read_address
             ; read_enable
             }
          |]
        ()
    with
    | [| data |] ->
      let { Line.valid; data; tag } = Line.Of_signal.unpack data in
      { O.data; hit = valid &: (tag ==: Params.address_to_tag read_address) }
    | _ -> assert false
  ;;

  let hierarchical ~name scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name (create ~name)
  ;;
end
