open! Core
open Hardcaml

(* TODO add boot ROM *)

module Size = struct
  module Enum = struct
    type t =
      | Byte
      | Half_word
      | Word
    [@@deriving sexp_of, compare, enumerate]
  end

  include Interface.Make_enums (Enum)
end

module I = struct
  type 'a t =
    { clock : 'a
    ; load_instruction : 'a
    ; load : 'a
    ; store : 'a
    ; program_counter : 'a [@bits Parameters.word_size]
    ; data_address : 'a [@bits Parameters.word_size]
    ; data_size : 'a Size.Binary.t
    ; data : 'a [@bits Parameters.word_size] [@rtlname "data_in"]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { instruction : 'a [@bits 32]
    ; data : 'a [@bits Parameters.word_size] [@rtlname "data_out"]
    ; error : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

let single_exn l =
  match l with
  | [ x ] -> x
  | _ -> invalid_argf "List.single_exn called on list of length %d" (List.length l) ()
;;

let match_on_size ~size ops =
  List.zip_exn Size.Enum.all ops |> Size.Binary.Of_signal.match_ size
;;

let choose_bank ~bank ~bank_selector ~size =
  let open Signal in
  List.init
    (width bank_selector + 1)
    ~f:(fun shift -> srl bank_selector shift ==:. Int.shift_right_logical bank shift)
  |> match_on_size ~size
;;

let split_data ~bank ~data ~size =
  let open Signal in
  List.init (List.length Size.Enum.all) ~f:(fun shift ->
    List.nth_exn
      (split_lsb ~part_width:8 (sel_bottom data (Int.shift_left 8 shift)))
      (bank land (Int.shift_left 1 shift - 1)))
  |> match_on_size ~size
;;

let combine_data ~bank_selector ~data ~size =
  let open Signal in
  List.init
    (width bank_selector + 1)
    ~f:(fun shift ->
      let bytes =
        List.chunks_of data ~length:(Int.shift_left 1 shift) |> List.map ~f:concat_lsb
      in
      (if width bank_selector = shift
      then single_exn bytes
      else mux (drop_bottom bank_selector shift) bytes)
      |> Fn.flip uresize (8 * List.length data))
  |> match_on_size ~size
;;

let is_unaligned_address ~size address =
  let open Signal in
  List.init (List.length Size.Enum.all) ~f:(fun shift ->
    if shift = 0 then gnd else sel_bottom address shift <>:. 0)
  |> match_on_size ~size
;;

module MakeRam (Params : sig
  val size : int
end) =
struct
  let address_bits = Signal.address_bits_for Params.size

  module Address = struct
    type 'a t =
      { address : 'a [@bits address_bits]
      ; size : 'a Size.Binary.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; read_address : 'a Address.t
      ; write_address : 'a Address.t
      ; read_enable : 'a
      ; write_enable : 'a
      ; write_data : 'a [@bits Parameters.word_size]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let ram
    ~name
    { I.clock
    ; read_address = { Address.address = read_address; size = read_size }
    ; write_address = { Address.address = write_address; size = write_size }
    ; read_enable
    ; write_enable
    ; write_data
    }
    =
    let open Signal in
    let bytes = width write_data / 8 in
    let bank_selector address = sel_bottom address (address_bits_for bytes) in
    let bank_address address = srl address (address_bits_for bytes) in
    match
      Array.init bytes ~f:(fun bank ->
        Ram.create
          ~name
          ~collision_mode:Read_before_write
          ~size:(Params.size / bytes)
          ~write_ports:
            [| { Ram.Write_port.write_clock = clock
               ; write_address = bank_address write_address
               ; write_enable =
                   write_enable
                   &: choose_bank
                        ~bank
                        ~bank_selector:(bank_selector write_address)
                        ~size:write_size
               ; write_data = split_data ~bank ~data:write_data ~size:write_size
               }
            |]
          ~read_ports:
            [| { Ram.Read_port.read_clock = clock
               ; read_address = bank_address read_address
               ; read_enable =
                   read_enable
                   &: choose_bank
                        ~bank
                        ~bank_selector:(bank_selector read_address)
                        ~size:read_size
               }
            |]
          ())
      |> Array.transpose_exn
      |> Array.map ~f:Array.to_list
    with
    | [| data |] ->
      combine_data ~bank_selector:(bank_selector read_address) ~data ~size:read_size
    | _ -> assert false
  ;;
end

module Segment = struct
  type 'a t =
    { is_pc_in_range : 'a
    ; is_data_address_in_range : 'a
    ; data : 'a [@bits Parameters.word_size]
    ; error : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

let ram
  ~size
  ~name
  ~is_in_range
  ~route
  { I.clock
  ; load_instruction
  ; load
  ; store
  ; program_counter
  ; data_address
  ; data_size
  ; data
  }
  =
  let open Signal in
  let module Ram =
    MakeRam (struct
      let size = size
    end)
  in
  let is_pc_in_range, is_data_address_in_range =
    is_in_range program_counter, is_in_range data_address
  in
  let load_instruction = load_instruction &: is_pc_in_range in
  let load = load &: is_data_address_in_range in
  { Segment.is_pc_in_range
  ; is_data_address_in_range
  ; data =
      (let program_counter, data_address =
         let route address = route address ~bits:Ram.address_bits in
         route program_counter, route data_address
       in
       Ram.ram
         ~name
         { Ram.I.clock
         ; read_address =
             { Ram.Address.address = mux2 load data_address program_counter
             ; size = Size.Binary.Of_signal.(mux2 load data_size (of_enum Word))
             }
         ; write_address = { Ram.Address.address = data_address; size = data_size }
         ; read_enable = load_instruction |: load
         ; write_enable = store &: is_data_address_in_range
         ; write_data = data
         })
  ; error = load_instruction &: load
  }
;;

let create
  _scope
  ({ I.clock = _
   ; load_instruction
   ; load
   ; store
   ; program_counter
   ; data_address
   ; data_size
   ; data = _
   } as i)
  =
  let open Signal in
  let segments =
    [ ram
        ~size:Parameters.imem_size
        ~name:"imem"
        ~is_in_range:(fun address ->
          Parameters.(address >=:. code_bottom &: (address <:. code_bottom + imem_size)))
        ~route:(fun address ~bits -> uresize (address -:. Parameters.code_bottom) bits)
        i
    ; ram
        ~size:Parameters.dmem_size
        ~name:"dmem"
        ~is_in_range:(fun address ->
          Parameters.(address >=:. stack_top - dmem_size &: (address <:. stack_top)))
        ~route:(fun address ~bits ->
          uresize (address -:. Parameters.(stack_top - dmem_size)) bits)
        i
    ]
  in
  let fold_data is_in_range =
    List.fold
      ~init:(List.hd_exn segments).data
      segments
      ~f:(fun acc ({ Segment.data; _ } as segment) -> mux2 (is_in_range segment) data acc)
  in
  { O.instruction = fold_data (fun { Segment.is_pc_in_range; _ } -> is_pc_in_range)
  ; data =
      fold_data (fun { Segment.is_data_address_in_range; _ } -> is_data_address_in_range)
  ; error =
      (List.map segments ~f:(fun { Segment.error; _ } -> error)
      @ [ is_unaligned_address ~size:data_size data_address &: (load |: store)
        ; is_unaligned_address ~size:(Size.Binary.Of_signal.of_enum Word) program_counter
          &: load_instruction
        ]
      @
      let any_in_range ranges = List.reduce_exn ranges ~f:( |: ) in
      [ ~:(List.map segments ~f:(fun { Segment.is_pc_in_range; _ } -> is_pc_in_range)
          |> any_in_range)
        &: load_instruction
      ; ~:(List.map segments ~f:(fun { Segment.is_data_address_in_range; _ } ->
             is_data_address_in_range)
          |> any_in_range)
        &: (load |: store)
      ])
      |> List.reduce_exn ~f:( |: )
  }
;;

let circuit scope =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"memory_controller" create
;;

module Tests = struct
  module Simulator = Cyclesim.With_interface (I) (O)
  module Waveform = Hardcaml_waveterm.Waveform

  let test_bench (sim : (_ I.t, _ O.t) Cyclesim.t) f =
    let open Bits in
    let inputs, outputs = Cyclesim.inputs sim, Cyclesim.outputs sim in
    let step () =
      Cyclesim.cycle sim;
      Stdio.print_s
        (let pretty p = to_int !p |> Printf.sprintf "0x%x" in
         [%sexp_of: string I.t * string O.t]
           (I.map inputs ~f:pretty, O.map outputs ~f:pretty));
      Stdio.print_endline ""
    in
    Cyclesim.reset sim;
    inputs.data := of_int ~width:Parameters.word_size 0xdeadbeef;
    inputs.data_address
      := of_int ~width:Parameters.word_size (Parameters.stack_top - Parameters.word_size);
    inputs.program_counter := of_int ~width:Parameters.word_size Parameters.code_bottom;
    inputs.store := gnd;
    inputs.load := gnd;
    inputs.load_instruction := gnd;
    Size.Binary.sim_set inputs.data_size Word;
    f (step, inputs)
  ;;

  let sim f =
    let scope = Scope.create ~flatten_design:true () in
    let sim = Simulator.create ~config:Cyclesim.Config.trace_all (create scope) in
    test_bench sim f
  ;;

  let%expect_test "Basic" =
    sim (fun (step, inputs) ->
      let open Bits in
      step ();
      inputs.store := vdd;
      step ();
      inputs.store := gnd;
      step ();
      inputs.load := vdd;
      step ();
      inputs.load := gnd;
      inputs.data_address := !(inputs.program_counter);
      inputs.store := vdd;
      step ();
      inputs.store := gnd;
      inputs.load_instruction := vdd;
      step ();
      inputs.load_instruction := gnd;
      inputs.store := vdd;
      step ());
    [%expect
      {|
      (((clock 0x0) (load_instruction 0x0) (load 0x0) (store 0x0)
        (program_counter 0x4000) (data_address 0x7fffffe0) (data_size 0x2)
        (data 0xdeadbeef))
       ((instruction 0x0) (data 0x0) (error 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x0) (store 0x1)
        (program_counter 0x4000) (data_address 0x7fffffe0) (data_size 0x2)
        (data 0xdeadbeef))
       ((instruction 0x0) (data 0x0) (error 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x0) (store 0x0)
        (program_counter 0x4000) (data_address 0x7fffffe0) (data_size 0x2)
        (data 0xdeadbeef))
       ((instruction 0x0) (data 0x0) (error 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x4000) (data_address 0x7fffffe0) (data_size 0x2)
        (data 0xdeadbeef))
       ((instruction 0x0) (data 0xdeadbeef) (error 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x0) (store 0x1)
        (program_counter 0x4000) (data_address 0x4000) (data_size 0x2)
        (data 0xdeadbeef))
       ((instruction 0x0) (data 0x0) (error 0x0)))

      (((clock 0x0) (load_instruction 0x1) (load 0x0) (store 0x0)
        (program_counter 0x4000) (data_address 0x4000) (data_size 0x2)
        (data 0xdeadbeef))
       ((instruction 0xdeadbeef) (data 0xdeadbeef) (error 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x0) (store 0x1)
        (program_counter 0x4000) (data_address 0x4000) (data_size 0x2)
        (data 0xdeadbeef))
       ((instruction 0xdeadbeef) (data 0xdeadbeef) (error 0x0))) |}]
  ;;

  let%expect_test "Sizes" =
    sim (fun (step, inputs) ->
      let open Bits in
      let base_address = !(inputs.data_address) in
      let offset_address n = inputs.data_address := base_address +:. n in
      inputs.store := vdd;
      step ();
      inputs.store := gnd;
      inputs.load := vdd;
      step ();
      Size.Binary.sim_set inputs.data_size Byte;
      step ();
      offset_address 1;
      step ();
      offset_address 2;
      step ();
      offset_address 3;
      step ();
      offset_address 4;
      step ();
      offset_address 0;
      Size.Binary.sim_set inputs.data_size Half_word;
      step ();
      offset_address 2;
      step ();
      offset_address 4;
      step ();
      offset_address 0;
      Size.Binary.sim_set inputs.data_size Word;
      step ();
      offset_address 1;
      Size.Binary.sim_set inputs.data_size Byte;
      inputs.load := gnd;
      inputs.store := vdd;
      inputs.data := of_int ~width:Parameters.word_size 0x69;
      step ();
      inputs.load := vdd;
      inputs.store := gnd;
      offset_address 0;
      Size.Binary.sim_set inputs.data_size Word;
      step ());
    [%expect
      {|
      (((clock 0x0) (load_instruction 0x0) (load 0x0) (store 0x1)
        (program_counter 0x4000) (data_address 0x7fffffe0) (data_size 0x2)
        (data 0xdeadbeef))
       ((instruction 0x0) (data 0x0) (error 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x4000) (data_address 0x7fffffe0) (data_size 0x2)
        (data 0xdeadbeef))
       ((instruction 0x0) (data 0xdeadbeef) (error 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x4000) (data_address 0x7fffffe0) (data_size 0x0)
        (data 0xdeadbeef))
       ((instruction 0x0) (data 0xef) (error 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x4000) (data_address 0x7fffffe1) (data_size 0x0)
        (data 0xdeadbeef))
       ((instruction 0x0) (data 0xbe) (error 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x4000) (data_address 0x7fffffe2) (data_size 0x0)
        (data 0xdeadbeef))
       ((instruction 0x0) (data 0xad) (error 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x4000) (data_address 0x7fffffe3) (data_size 0x0)
        (data 0xdeadbeef))
       ((instruction 0x0) (data 0xde) (error 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x4000) (data_address 0x7fffffe4) (data_size 0x0)
        (data 0xdeadbeef))
       ((instruction 0x0) (data 0x0) (error 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x4000) (data_address 0x7fffffe0) (data_size 0x1)
        (data 0xdeadbeef))
       ((instruction 0x0) (data 0xbeef) (error 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x4000) (data_address 0x7fffffe2) (data_size 0x1)
        (data 0xdeadbeef))
       ((instruction 0x0) (data 0xdead) (error 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x4000) (data_address 0x7fffffe4) (data_size 0x1)
        (data 0xdeadbeef))
       ((instruction 0x0) (data 0x0) (error 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x4000) (data_address 0x7fffffe0) (data_size 0x2)
        (data 0xdeadbeef))
       ((instruction 0x0) (data 0xdeadbeef) (error 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x0) (store 0x1)
        (program_counter 0x4000) (data_address 0x7fffffe1) (data_size 0x0)
        (data 0x69))
       ((instruction 0x0) (data 0xdeadbeef) (error 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x4000) (data_address 0x7fffffe0) (data_size 0x2)
        (data 0x69))
       ((instruction 0x0) (data 0xdead69ef) (error 0x0))) |}]
  ;;

  let%expect_test "Invalid addresses" =
    sim (fun (step, inputs) ->
      let open Bits in
      inputs.data_address
        := of_int ~width:Parameters.word_size (Parameters.code_bottom - 1);
      step ();
      inputs.load := vdd;
      step ();
      inputs.data_address
        := of_int
             ~width:Parameters.word_size
             (Parameters.code_bottom + Parameters.imem_size);
      step ();
      inputs.data_address := of_int ~width:Parameters.word_size Parameters.stack_top;
      step ();
      inputs.data_address
        := of_int
             ~width:Parameters.word_size
             (Parameters.stack_top - Parameters.dmem_size - 1);
      step ();
      inputs.data_address
        := of_int ~width:Parameters.word_size (Parameters.stack_top - Parameters.dmem_size);
      step ());
    [%expect
      {|
      (((clock 0x0) (load_instruction 0x0) (load 0x0) (store 0x0)
        (program_counter 0x4000) (data_address 0x3fff) (data_size 0x2)
        (data 0xdeadbeef))
       ((instruction 0x0) (data 0x0) (error 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x4000) (data_address 0x3fff) (data_size 0x2)
        (data 0xdeadbeef))
       ((instruction 0x0) (data 0x0) (error 0x1)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x4000) (data_address 0x14000) (data_size 0x2)
        (data 0xdeadbeef))
       ((instruction 0x0) (data 0x0) (error 0x1)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x4000) (data_address 0x80000000) (data_size 0x2)
        (data 0xdeadbeef))
       ((instruction 0x0) (data 0x0) (error 0x1)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x4000) (data_address 0x7ffeffff) (data_size 0x2)
        (data 0xdeadbeef))
       ((instruction 0x0) (data 0x0) (error 0x1)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x4000) (data_address 0x7fff0000) (data_size 0x2)
        (data 0xdeadbeef))
       ((instruction 0x0) (data 0x0) (error 0x0))) |}]
  ;;

  let%expect_test "Unaligned addresses" =
    sim (fun (step, inputs) ->
      let open Bits in
      inputs.load := vdd;
      List.map Size.Enum.all ~f:(fun s ->
        List.init (Parameters.word_size / 8) ~f:(fun i -> s, i))
      |> List.concat
      |> List.iter ~f:(fun (s, offset) ->
           inputs.data_address
             := of_int ~width:Parameters.word_size (Parameters.code_bottom + offset);
           Size.Binary.sim_set inputs.data_size s;
           step ()));
    [%expect
      {|
      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x4000) (data_address 0x4000) (data_size 0x0)
        (data 0xdeadbeef))
       ((instruction 0x0) (data 0x0) (error 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x4000) (data_address 0x4001) (data_size 0x0)
        (data 0xdeadbeef))
       ((instruction 0x0) (data 0x0) (error 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x4000) (data_address 0x4002) (data_size 0x0)
        (data 0xdeadbeef))
       ((instruction 0x0) (data 0x0) (error 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x4000) (data_address 0x4003) (data_size 0x0)
        (data 0xdeadbeef))
       ((instruction 0x0) (data 0x0) (error 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x4000) (data_address 0x4000) (data_size 0x1)
        (data 0xdeadbeef))
       ((instruction 0x0) (data 0x0) (error 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x4000) (data_address 0x4001) (data_size 0x1)
        (data 0xdeadbeef))
       ((instruction 0x0) (data 0x0) (error 0x1)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x4000) (data_address 0x4002) (data_size 0x1)
        (data 0xdeadbeef))
       ((instruction 0x0) (data 0x0) (error 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x4000) (data_address 0x4003) (data_size 0x1)
        (data 0xdeadbeef))
       ((instruction 0x0) (data 0x0) (error 0x1)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x4000) (data_address 0x4000) (data_size 0x2)
        (data 0xdeadbeef))
       ((instruction 0x0) (data 0x0) (error 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x4000) (data_address 0x4001) (data_size 0x2)
        (data 0xdeadbeef))
       ((instruction 0x0) (data 0x0) (error 0x1)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x4000) (data_address 0x4002) (data_size 0x2)
        (data 0xdeadbeef))
       ((instruction 0x0) (data 0x0) (error 0x1)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x4000) (data_address 0x4003) (data_size 0x2)
        (data 0xdeadbeef))
       ((instruction 0x0) (data 0x0) (error 0x1))) |}]
  ;;
end
