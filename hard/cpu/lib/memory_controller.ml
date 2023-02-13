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

let address_bits = Signal.address_bits_for Parameters.(imem_size + dmem_size)

module Route = struct
  type 'a t =
    { address : 'a [@bits address_bits]
    ; error : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module Ram_address = struct
  type 'a t =
    { address : 'a [@bits address_bits]
    ; size : 'a Size.Binary.t
    }
  [@@deriving sexp_of, hardcaml]
end

let split_data ~bank ~bank_selector ~data ~size =
  let open Signal in
  ( Size.Binary.Of_signal.match_
      size
      [ Byte, bank_selector ==:. bank
      ; Half_word, srl bank_selector 1 ==:. Int.shift_right_logical bank 1
      ; Word, vdd
      ]
  , Size.Binary.Of_signal.match_
      size
      [ Byte, sel_bottom data 8
      ; ( Half_word
        , List.nth_exn (split_lsb ~part_width:8 (sel_bottom data 16)) (bank land 1) )
      ; Word, List.nth_exn (split_lsb ~part_width:8 data) bank
      ] )
;;

let combine_data ~bank_selector ~data ~size =
  let open Signal in
  Size.Binary.Of_signal.match_
    size
    ([ Size.Enum.Byte, mux bank_selector data
     ; ( Half_word
       , mux (msbs bank_selector) (List.chunks_of data ~length:2 |> List.map ~f:concat_lsb)
       )
     ; Word, concat_lsb data
     ]
    |> List.map ~f:(fun (size, data) -> size, uresize data Parameters.word_size))
;;

let ram
  ~clock
  ~write_address:{ Ram_address.address = write_address; size = write_size }
  ~read_address1:{ Ram_address.address = read_address1; size = read_size1 }
  ~read_address2:{ Ram_address.address = read_address2; size = read_size2 }
  ~write_enable
  ~read_enable1
  ~read_enable2
  ~write_data
  =
  let open Signal in
  let _checks =
    assert (width write_address = address_bits);
    assert (width read_address1 = address_bits);
    assert (width read_address2 = address_bits);
    assert (width write_data = Parameters.word_size)
  in
  let bytes = Parameters.word_size / 8 in
  let bank_selector address = sel_bottom address (address_bits_for bytes) in
  let bank_address address = srl address (address_bits_for bytes) in
  match
    Array.init bytes ~f:(fun bank ->
      Ram.create
        ~name:"memory"
        ~collision_mode:Read_before_write
        ~size:(Parameters.(imem_size + dmem_size) / bytes)
        ~write_ports:
          [| (let write_enable, write_data =
                split_data
                  ~bank
                  ~bank_selector:(bank_selector write_address)
                  ~data:write_data
                  ~size:write_size
                |> Tuple2.map_fst ~f:(( &: ) write_enable)
              in
              { Ram.Write_port.write_clock = clock
              ; write_address = bank_address write_address
              ; write_enable
              ; write_data
              })
          |]
        ~read_ports:
          [| { Ram.Read_port.read_clock = clock
             ; read_address = bank_address read_address1
             ; read_enable = read_enable1
             }
           ; { Ram.Read_port.read_clock = clock
             ; read_address = bank_address read_address2
             ; read_enable = read_enable2
             }
          |]
        ())
    |> Array.transpose_exn
    |> Array.map ~f:Array.to_list
  with
  | [| data1; data2 |] ->
    ( combine_data
        ~bank_selector:(bank_selector read_address1)
        ~data:data1
        ~size:read_size1
    , combine_data
        ~bank_selector:(bank_selector read_address2)
        ~data:data2
        ~size:read_size2 )
  | _ -> assert false
;;

let router ~address ~size =
  let open Signal in
  let routed_address = Always.Variable.wire ~default:(zero address_bits) in
  let invalid_address = Always.Variable.wire ~default:gnd in
  let unaligned_address = Always.Variable.wire ~default:gnd in
  Always.(
    compile
      [ Parameters.(
          if_
            (address >=:. stack_top - dmem_size &: (address <:. stack_top))
            [ routed_address
              <-- uresize (address -:. (stack_top - dmem_size)) address_bits
            ]
          @@ elif
               (address >=:. code_bottom &: (address <:. code_bottom + imem_size))
               [ routed_address <-- uresize (address -:. code_bottom) address_bits ]
          @@ [ invalid_address <-- vdd ])
      ; Size.Binary.Of_always.match_
          size
          ([ Size.Enum.Byte, gnd
           ; Half_word, sel_bottom address 1
           ; Word, sel_bottom address 2
           ]
          |> List.map ~f:(fun (size, alignment) ->
               size, [ unaligned_address <-- (alignment <>:. 0) ]))
      ]);
  { Route.address = routed_address.value
  ; error = invalid_address.value |: unaligned_address.value
  }
;;

let create
  (scope : Scope.t)
  ({ clock
   ; load_instruction
   ; load
   ; store
   ; program_counter
   ; data_address
   ; data_size
   ; data
   } :
    _ I.t)
  =
  let open Signal in
  let routed_pc = wire address_bits in
  let routed_data_address = wire address_bits in
  let raw_instruction, data_out =
    let routed_data_address =
      { Ram_address.address = routed_data_address; size = data_size }
    in
    ram
      ~clock
      ~write_address:routed_data_address
      ~read_address1:
        { Ram_address.address = routed_pc; size = Size.Binary.Of_signal.of_enum Word }
      ~read_address2:routed_data_address
      ~write_enable:store
      ~read_enable1:load_instruction
      ~read_enable2:load
      ~write_data:data
  in
  let { Route.address = pc; error = pc_error } =
    router ~address:program_counter ~size:(Size.Binary.Of_signal.of_enum Word)
  in
  routed_pc <== pc;
  let { Route.address = data_address; error = data_error } =
    router ~address:data_address ~size:data_size
  in
  routed_data_address <== data_address;
  let _debugging =
    let ( -- ) = Scope.naming scope in
    ignore (routed_pc -- "routed_pc");
    ignore (routed_data_address -- "routed_data_address");
    ignore (pc -- "pc");
    ignore (data_address -- "data_address");
    ignore (pc_error -- "pc_error");
    ignore (data_error -- "data_error");
    ignore (raw_instruction -- "raw_instruction");
    ignore (data_out -- "data_out")
  in
  { O.instruction = raw_instruction
  ; data = data_out
  ; error = pc_error &: load_instruction |: (data_error &: (load |: store))
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
       ((instruction 0x0) (data 0xdeadbeef) (error 0x0)))

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
       ((instruction 0x0) (data 0xbe) (error 0x0)))

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
