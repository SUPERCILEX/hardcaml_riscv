open! Core
open Hardcaml

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
    ; write_data : 'a [@bits Parameters.word_size]
    ; uart : 'a Uart.I.t [@rtlmangle true]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { instruction : 'a [@bits 32]
    ; read_data : 'a [@bits Parameters.word_size]
    ; error : 'a
    ; uart : 'a Uart.O.t [@rtlmangle true]
    ; stall : 'a
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
          ~name:(Printf.sprintf "%s_%d" name bank)
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
      let spec = Reg_spec.create ~clock () in
      combine_data
        ~bank_selector:(bank_selector read_address |> reg ~enable:read_enable spec)
        ~data
        ~size:(Size.Binary.Of_signal.reg ~enable:read_enable spec read_size)
    | _ -> assert false
  ;;
end

module Segment = struct
  type 'a t =
    { read_data : 'a [@bits Parameters.word_size]
    ; error : 'a
    ; stall : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module Local_ram = struct
  let create
    _scope
    ~name
    ~size
    { I.clock
    ; load_instruction
    ; load
    ; store
    ; program_counter
    ; data_address
    ; data_size
    ; write_data
    ; uart = _
    }
    =
    let open Signal in
    let module Ram =
      MakeRam (struct
        let size = size
      end)
    in
    { Segment.read_data =
        Ram.ram
          ~name
          { Ram.I.clock
          ; read_address =
              { Ram.Address.address = mux2 load data_address program_counter
              ; size = Size.Binary.Of_signal.(mux2 load data_size (of_enum Word))
              }
          ; write_address = { Ram.Address.address = data_address; size = data_size }
          ; read_enable = load_instruction |: load
          ; write_enable = store
          ; write_data
          }
    ; error = load_instruction &: load
    ; stall = gnd
    }
  ;;

  let circuit scope ~name ~size =
    let module H = Hierarchy.In_scope (I) (Segment) in
    let module D = Debugging.In_scope (I) (Segment) in
    H.hierarchical ~scope ~name (D.create ~create_fn:(create ~name ~size))
  ;;
end

module Rom = struct
  let create
    _scope
    ~data
    { I.clock
    ; load_instruction
    ; load
    ; store
    ; program_counter
    ; data_address
    ; data_size
    ; write_data = _
    ; uart = _
    }
    =
    let open Signal in
    List.iter data ~f:(fun data -> assert (width data = 8));
    { Segment.read_data =
        (let bytes = 4 in
         let address = mux2 load data_address program_counter in
         let spec = Reg_spec.create ~clock () in
         let enable = load_instruction |: load in
         combine_data
           ~bank_selector:(sel_bottom address (address_bits_for bytes) |> reg ~enable spec)
           ~data:
             (List.chunks_of ~length:bytes data
             |> List.transpose_exn
             |> List.map ~f:(fun byte_bank ->
                  mux (srl address (address_bits_for bytes)) byte_bank |> reg ~enable spec)
             )
           ~size:
             Size.Binary.Of_signal.(
               mux2 load data_size (of_enum Word) |> reg ~enable spec))
    ; error = store |: (load_instruction &: load)
    ; stall = gnd
    }
  ;;

  let circuit scope ~name ~data =
    let module H = Hierarchy.In_scope (I) (Segment) in
    let module D = Debugging.In_scope (I) (Segment) in
    H.hierarchical ~scope ~name (D.create ~create_fn:(create ~data))
  ;;
end

module Uart_io = struct
  module O = struct
    type 'a t =
      { uart : 'a Uart.O.t [@rtlmangle true]
      ; segment : 'a Segment.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create
    _scope
    { I.clock
    ; load_instruction
    ; load
    ; store
    ; program_counter = _
    ; data_address = _
    ; data_size
    ; write_data
    ; uart = { Uart.I.write_done; read_data; read_done }
    }
    =
    let open Signal in
    { O.uart =
        { Uart.O.write_data = sel_bottom write_data 8
        ; write_ready = store
        ; read_ready = load
        }
    ; segment =
        { Segment.read_data =
            uresize read_data Parameters.word_size
            |> reg ~enable:load (Reg_spec.create ~clock ())
        ; error =
            [ load_instruction
            ; ~:(Size.Binary.Of_signal.is data_size Byte) &: (load |: store)
            ]
            |> List.reduce_exn ~f:( |: )
        ; stall =
            [ store &: ~:write_done; load &: ~:read_done ] |> List.reduce_exn ~f:( |: )
        }
    }
  ;;

  let circuit scope =
    let module H = Hierarchy.In_scope (I) (O) in
    let module D = Debugging.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"uart_io" (D.create ~create_fn:create)
  ;;
end

let create
  scope
  ~bootloader
  { I.clock
  ; load_instruction
  ; load
  ; store
  ; program_counter
  ; data_address
  ; data_size
  ; write_data
  ; uart
  }
  =
  let open Signal in
  let segments, uart_out =
    let build_input ~is_in_range ~route =
      { I.clock
      ; load_instruction = load_instruction &: is_in_range program_counter
      ; load = load &: is_in_range data_address
      ; store = store &: is_in_range data_address
      ; program_counter = route program_counter
      ; data_address = route data_address
      ; data_size
      ; write_data
      ; uart
      }
    in
    let mem_segments =
      [ (let open Parameters in
        let size = imem_size in
        let input =
          build_input
            ~is_in_range:(fun address ->
              address >=:. code_bottom &: (address <:. code_bottom + imem_size))
            ~route:(code_bottom |> Fn.flip ( -:. ))
        in
        input, Local_ram.circuit scope ~size ~name:"imem" input)
      ; (let open Parameters in
        let size = dmem_size in
        let input =
          build_input
            ~is_in_range:(fun address ->
              address >=:. stack_top - dmem_size &: (address <:. stack_top))
            ~route:(stack_top - dmem_size |> Fn.flip ( -:. ))
        in
        input, Local_ram.circuit scope ~size ~name:"dmem" input)
      ; (let open Parameters in
        let input =
          build_input
            ~is_in_range:(fun address ->
              address
              >=:. bootloader_start
              &: (address <:. bootloader_start + List.length bootloader))
            ~route:(bootloader_start |> Fn.flip ( -:. ))
        in
        input, Rom.circuit scope ~name:"bootloader" ~data:bootloader input)
      ]
    in
    let uart_segment, uart_out =
      let open Parameters in
      let input =
        build_input
          ~is_in_range:(uart_io_address |> Fn.flip ( ==:. ))
          ~route:(fun _ -> zero word_size)
      in
      let Uart_io.O.{ uart = uart_out; segment } = Uart_io.circuit scope input in
      (input, segment), uart_out
    in
    mem_segments @ [ uart_segment ], uart_out
  in
  let read_data activator =
    mux
      (let activations = List.map segments ~f:activator in
       List.mapi activations ~f:(fun i active ->
         sresize active (address_bits_for (List.length segments)) &:. i)
       |> List.reduce_exn ~f:( |: )
       |> reg ~enable:(List.reduce_exn activations ~f:( |: )) (Reg_spec.create ~clock ()))
      (List.map segments ~f:(fun (_, { Segment.read_data; _ }) -> read_data))
  in
  { O.instruction = read_data (fun ({ I.load_instruction; _ }, _) -> load_instruction)
  ; read_data = read_data (fun ({ I.load; _ }, _) -> load)
  ; error =
      List.map segments ~f:(fun (_, { Segment.error; _ }) -> error)
      @ [ is_unaligned_address ~size:data_size data_address &: (load |: store)
        ; is_unaligned_address ~size:(Size.Binary.Of_signal.of_enum Word) program_counter
          &: load_instruction
        ]
      @ [ ~:(List.map segments ~f:(fun ({ I.load_instruction; _ }, _) -> load_instruction)
            |> List.reduce_exn ~f:( |: ))
          &: load_instruction
        ; ~:(List.map segments ~f:(fun ({ I.load; store; _ }, _) -> load |: store)
            |> List.reduce_exn ~f:( |: ))
          &: (load |: store)
        ]
      |> List.reduce_exn ~f:( |: )
  ; uart = uart_out
  ; stall =
      List.map segments ~f:(fun (_, { Segment.stall; _ }) -> stall)
      |> List.reduce_exn ~f:( |: )
  }
;;

let circuit scope ~bootloader =
  let module H = Hierarchy.In_scope (I) (O) in
  let module D = Debugging.In_scope (I) (O) in
  H.hierarchical
    ~scope
    ~name:"memory_controller"
    (D.create ~create_fn:(create ~bootloader))
;;

module Tests = struct
  let test_bench (sim : (_ I.t, _ O.t) Cyclesim.t) ~f =
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
    let open Parameters in
    inputs.write_data := of_int ~width:word_size 0xdeadbeef;
    inputs.data_address := of_int ~width:word_size (stack_top - word_size);
    inputs.program_counter := of_int ~width:word_size code_bottom;
    Size.Binary.sim_set inputs.data_size Word;
    f ~step ~inputs
  ;;

  let sim f =
    let module Simulator = Cyclesim.With_interface (I) (O) in
    let scope = Scope.create ~flatten_design:true () in
    let fake_rom = List.init 80 ~f:(Signal.of_int ~width:8) in
    Simulator.create ~config:Cyclesim.Config.trace_all (create scope ~bootloader:fake_rom)
    |> test_bench ~f
  ;;

  let%expect_test "Basic" =
    sim (fun ~step ~inputs ->
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
        (program_counter 0x100000) (data_address 0x7fffffe0) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x0) (store 0x1)
        (program_counter 0x100000) (data_address 0x7fffffe0) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x0) (store 0x0)
        (program_counter 0x100000) (data_address 0x7fffffe0) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x7fffffe0) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0xdeadbeef) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x0) (store 0x1)
        (program_counter 0x100000) (data_address 0x100000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0xdeadbeef) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x1) (load 0x0) (store 0x0)
        (program_counter 0x100000) (data_address 0x100000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0xdeadbeef) (read_data 0xdeadbeef) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x0) (store 0x1)
        (program_counter 0x100000) (data_address 0x100000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0xdeadbeef) (read_data 0xdeadbeef) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0))) |}]
  ;;

  let%expect_test "Sizes" =
    sim (fun ~step ~inputs ->
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
      inputs.write_data := of_int ~width:Parameters.word_size 0x69;
      step ();
      inputs.load := vdd;
      inputs.store := gnd;
      offset_address 0;
      Size.Binary.sim_set inputs.data_size Word;
      step ());
    [%expect
      {|
      (((clock 0x0) (load_instruction 0x0) (load 0x0) (store 0x1)
        (program_counter 0x100000) (data_address 0x7fffffe0) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x7fffffe0) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0xdeadbeef) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x7fffffe0) (data_size 0x0)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0xef) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x7fffffe1) (data_size 0x0)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0xbe) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x7fffffe2) (data_size 0x0)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0xad) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x7fffffe3) (data_size 0x0)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0xde) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x7fffffe4) (data_size 0x0)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x7fffffe0) (data_size 0x1)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0xbeef) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x7fffffe2) (data_size 0x1)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0xdead) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x7fffffe4) (data_size 0x1)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x7fffffe0) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0xdeadbeef) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x0) (store 0x1)
        (program_counter 0x100000) (data_address 0x7fffffe1) (data_size 0x0)
        (write_data 0x69)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0xdeadbeef) (error 0x0)
        (uart ((write_data 0x69) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x7fffffe0) (data_size 0x2)
        (write_data 0x69)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0xdead69ef) (error 0x0)
        (uart ((write_data 0x69) (write_ready 0x0) (read_ready 0x0))) (stall 0x0))) |}]
  ;;

  let%expect_test "Invalid addresses" =
    sim (fun ~step ~inputs ->
      let open Bits in
      let open Parameters in
      inputs.data_address := of_int ~width:word_size (code_bottom - 1);
      step ();
      inputs.load := vdd;
      step ();
      inputs.data_address := of_int ~width:word_size (code_bottom + imem_size);
      step ();
      inputs.data_address := of_int ~width:word_size stack_top;
      step ();
      inputs.data_address := of_int ~width:word_size (stack_top - dmem_size - 1);
      step ();
      inputs.data_address := of_int ~width:word_size (stack_top - dmem_size);
      step ());
    [%expect
      {|
      (((clock 0x0) (load_instruction 0x0) (load 0x0) (store 0x0)
        (program_counter 0x100000) (data_address 0xfffff) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0xfffff) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x1)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x110000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x1)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x80000000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x1)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x7ffeffff) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x1)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x7fff0000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0))) |}]
  ;;

  let%expect_test "Unaligned addresses" =
    sim (fun ~step ~inputs ->
      let open Bits in
      inputs.load := vdd;
      List.map Size.Enum.all ~f:(fun s ->
        List.init (Parameters.word_size / 8) ~f:(fun i -> s, i))
      |> List.concat
      |> List.iter ~f:(fun (s, offset) ->
           (inputs.data_address
              := Parameters.(of_int ~width:word_size (code_bottom + offset)));
           Size.Binary.sim_set inputs.data_size s;
           step ()));
    [%expect
      {|
      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x100000) (data_size 0x0)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x100001) (data_size 0x0)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x100002) (data_size 0x0)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x100003) (data_size 0x0)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x100000) (data_size 0x1)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x100001) (data_size 0x1)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x1)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x100002) (data_size 0x1)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x100003) (data_size 0x1)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x1)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x100000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x100001) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x1)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x100002) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x1)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x100003) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x1)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0))) |}]
  ;;

  let%expect_test "Overlapping ops" =
    sim (fun ~step ~inputs ->
      let open Bits in
      let open Parameters in
      let run () =
        Stdio.print_endline "--------------------------------------------";
        let permutations =
          let options = [ vdd; gnd ] in
          List.cartesian_product options options |> List.cartesian_product options
        in
        List.iter permutations ~f:(fun (load, (load_instruction, store)) ->
          inputs.load := load;
          inputs.load_instruction := load_instruction;
          inputs.store := store;
          step ())
      in
      inputs.data_address := of_int ~width:word_size code_bottom;
      inputs.program_counter := of_int ~width:word_size (code_bottom + 512);
      run ();
      inputs.data_address := of_int ~width:word_size (stack_top - dmem_size);
      inputs.program_counter := !(inputs.data_address);
      run ();
      inputs.data_address := of_int ~width:word_size bootloader_start;
      inputs.program_counter := of_int ~width:word_size (bootloader_start + 4);
      run ();
      inputs.data_address := of_int ~width:word_size (stack_top - dmem_size);
      inputs.program_counter := of_int ~width:word_size code_bottom;
      run ());
    [%expect
      {|
      --------------------------------------------
      (((clock 0x0) (load_instruction 0x1) (load 0x1) (store 0x1)
        (program_counter 0x100200) (data_address 0x100000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x1)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x1) (load 0x1) (store 0x0)
        (program_counter 0x100200) (data_address 0x100000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0xdeadbeef) (read_data 0xdeadbeef) (error 0x1)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x1)
        (program_counter 0x100200) (data_address 0x100000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0xdeadbeef) (read_data 0xdeadbeef) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100200) (data_address 0x100000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0xdeadbeef) (read_data 0xdeadbeef) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x1) (load 0x0) (store 0x1)
        (program_counter 0x100200) (data_address 0x100000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x1) (load 0x0) (store 0x0)
        (program_counter 0x100200) (data_address 0x100000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x0) (store 0x1)
        (program_counter 0x100200) (data_address 0x100000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x0) (store 0x0)
        (program_counter 0x100200) (data_address 0x100000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      --------------------------------------------
      (((clock 0x0) (load_instruction 0x1) (load 0x1) (store 0x1)
        (program_counter 0x7fff0000) (data_address 0x7fff0000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x1)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x1) (load 0x1) (store 0x0)
        (program_counter 0x7fff0000) (data_address 0x7fff0000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0xdeadbeef) (read_data 0xdeadbeef) (error 0x1)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x1)
        (program_counter 0x7fff0000) (data_address 0x7fff0000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0xdeadbeef) (read_data 0xdeadbeef) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x7fff0000) (data_address 0x7fff0000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0xdeadbeef) (read_data 0xdeadbeef) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x1) (load 0x0) (store 0x1)
        (program_counter 0x7fff0000) (data_address 0x7fff0000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0xdeadbeef) (read_data 0xdeadbeef) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x1) (load 0x0) (store 0x0)
        (program_counter 0x7fff0000) (data_address 0x7fff0000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0xdeadbeef) (read_data 0xdeadbeef) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x0) (store 0x1)
        (program_counter 0x7fff0000) (data_address 0x7fff0000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0xdeadbeef) (read_data 0xdeadbeef) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x0) (store 0x0)
        (program_counter 0x7fff0000) (data_address 0x7fff0000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0xdeadbeef) (read_data 0xdeadbeef) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      --------------------------------------------
      (((clock 0x0) (load_instruction 0x1) (load 0x1) (store 0x1)
        (program_counter 0x4004) (data_address 0x4000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x3020100) (read_data 0x3020100) (error 0x1)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x1) (load 0x1) (store 0x0)
        (program_counter 0x4004) (data_address 0x4000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x3020100) (read_data 0x3020100) (error 0x1)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x1)
        (program_counter 0x4004) (data_address 0x4000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x3020100) (read_data 0x3020100) (error 0x1)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x4004) (data_address 0x4000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x3020100) (read_data 0x3020100) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x1) (load 0x0) (store 0x1)
        (program_counter 0x4004) (data_address 0x4000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x7060504) (read_data 0x7060504) (error 0x1)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x1) (load 0x0) (store 0x0)
        (program_counter 0x4004) (data_address 0x4000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x7060504) (read_data 0x7060504) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x0) (store 0x1)
        (program_counter 0x4004) (data_address 0x4000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x7060504) (read_data 0x7060504) (error 0x1)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x0) (store 0x0)
        (program_counter 0x4004) (data_address 0x4000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x7060504) (read_data 0x7060504) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      --------------------------------------------
      (((clock 0x0) (load_instruction 0x1) (load 0x1) (store 0x1)
        (program_counter 0x100000) (data_address 0x7fff0000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0xdeadbeef) (read_data 0xdeadbeef) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x1) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x7fff0000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0xdeadbeef) (read_data 0xdeadbeef) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x1)
        (program_counter 0x100000) (data_address 0x7fff0000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0xdeadbeef) (read_data 0xdeadbeef) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x7fff0000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0xdeadbeef) (read_data 0xdeadbeef) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x1) (load 0x0) (store 0x1)
        (program_counter 0x100000) (data_address 0x7fff0000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0xdeadbeef) (read_data 0xdeadbeef) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x1) (load 0x0) (store 0x0)
        (program_counter 0x100000) (data_address 0x7fff0000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0xdeadbeef) (read_data 0xdeadbeef) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x0) (store 0x1)
        (program_counter 0x100000) (data_address 0x7fff0000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0xdeadbeef) (read_data 0xdeadbeef) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x0) (store 0x0)
        (program_counter 0x100000) (data_address 0x7fff0000) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0xdeadbeef) (read_data 0xdeadbeef) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0))) |}]
  ;;

  let%expect_test "Simple boot rom" =
    sim (fun ~step ~inputs ->
      let open Bits in
      Size.Binary.sim_set inputs.data_size Byte;
      (inputs.data_address := Parameters.(of_int ~width:word_size bootloader_start));
      inputs.load := vdd;
      List.init 4 ~f:Fn.id
      |> List.iter ~f:(fun _ ->
           inputs.data_address := !(inputs.data_address) +:. 1;
           step ());
      Stdio.print_endline "--------------------------------------------";
      Size.Binary.sim_set inputs.data_size Half_word;
      List.init 4 ~f:Fn.id
      |> List.iter ~f:(fun _ ->
           step ();
           inputs.data_address := !(inputs.data_address) +:. 2));
    [%expect
      {|
      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x4001) (data_size 0x0)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x1) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x4002) (data_size 0x0)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x2) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x4003) (data_size 0x0)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x3) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x4004) (data_size 0x0)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x4) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      --------------------------------------------
      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x4004) (data_size 0x1)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x504) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x4006) (data_size 0x1)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x706) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x4008) (data_size 0x1)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x908) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x400a) (data_size 0x1)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0xb0a) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0))) |}]
  ;;

  let%expect_test "UART errors" =
    sim (fun ~step ~inputs ->
      let open Bits in
      (inputs.data_address := Parameters.(of_int ~width:word_size uart_io_address));
      inputs.program_counter := !(inputs.data_address);
      inputs.load := vdd;
      List.iter (List.rev Size.Enum.all) ~f:(fun size ->
        Size.Binary.sim_set inputs.data_size size;
        step ());
      inputs.load := gnd;
      inputs.load_instruction := vdd;
      step ());
    [%expect
      {|
      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x2003) (data_address 0x2003) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x1)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x1))) (stall 0x1)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x2003) (data_address 0x2003) (data_size 0x1)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x1)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x1))) (stall 0x1)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x2003) (data_address 0x2003) (data_size 0x0)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x1))) (stall 0x1)))

      (((clock 0x0) (load_instruction 0x1) (load 0x0) (store 0x0)
        (program_counter 0x2003) (data_address 0x2003) (data_size 0x0)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x1)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0))) |}]
  ;;

  let%expect_test "UART" =
    sim (fun ~step ~inputs ->
      let open Bits in
      (inputs.data_address := Parameters.(of_int ~width:word_size uart_io_address));
      inputs.load := vdd;
      step ();
      step ();
      inputs.uart.read_done := vdd;
      step ();
      inputs.load := gnd;
      inputs.uart.read_done := gnd;
      step ();
      Stdio.print_endline "--------------------------------------------";
      inputs.store := vdd;
      step ();
      step ();
      inputs.uart.write_done := vdd;
      step ();
      inputs.store := gnd;
      inputs.uart.write_done := gnd;
      step ());
    [%expect
      {|
      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x2003) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x1)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x1))) (stall 0x1)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x2003) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x1)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x1))) (stall 0x1)))

      (((clock 0x0) (load_instruction 0x0) (load 0x1) (store 0x0)
        (program_counter 0x100000) (data_address 0x2003) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x1))))
       ((instruction 0x0) (read_data 0x0) (error 0x1)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x1))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x0) (store 0x0)
        (program_counter 0x100000) (data_address 0x2003) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0)))

      --------------------------------------------
      (((clock 0x0) (load_instruction 0x0) (load 0x0) (store 0x1)
        (program_counter 0x100000) (data_address 0x2003) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x1)
        (uart ((write_data 0xef) (write_ready 0x1) (read_ready 0x0))) (stall 0x1)))

      (((clock 0x0) (load_instruction 0x0) (load 0x0) (store 0x1)
        (program_counter 0x100000) (data_address 0x2003) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x1)
        (uart ((write_data 0xef) (write_ready 0x1) (read_ready 0x0))) (stall 0x1)))

      (((clock 0x0) (load_instruction 0x0) (load 0x0) (store 0x1)
        (program_counter 0x100000) (data_address 0x2003) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x1) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x1)
        (uart ((write_data 0xef) (write_ready 0x1) (read_ready 0x0))) (stall 0x0)))

      (((clock 0x0) (load_instruction 0x0) (load 0x0) (store 0x0)
        (program_counter 0x100000) (data_address 0x2003) (data_size 0x2)
        (write_data 0xdeadbeef)
        (uart ((write_done 0x0) (read_data 0x0) (read_done 0x0))))
       ((instruction 0x0) (read_data 0x0) (error 0x0)
        (uart ((write_data 0xef) (write_ready 0x0) (read_ready 0x0))) (stall 0x0))) |}]
  ;;
end
