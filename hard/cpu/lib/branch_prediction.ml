open! Core
open Hardcaml

let extract_high_entropy_bits_from_program_counter address =
  let open Signal in
  concat_msb
    [ address.:(Int.floor_log2 Parameters.code_bottom)
    ; sel_bottom address (Int.floor_log2 Parameters.imem_size) |> msbs
    ]
;;

let hash_program_counter ~bits address =
  let open Signal in
  let shuffle list =
    let random_prime =
      "155932701214303184326864131316095681640290247695109515520724340505777567578309211013555223897242597556615485113571634396826737509550588918899636445222600177152071201620397262999574183535311513714762158246551801564970296048152341668550491010639120717043516543640455187677926056798474846669042658878200255193621"
    in
    List.mapi list ~f:(fun i c ->
      String.slice random_prime i (i + 10) |> Int.of_string, c)
    |> List.sort ~compare:(fun (random1, _) (random2, _) -> Int.compare random1 random2)
    |> List.map ~f:snd
  in
  let useful_address = extract_high_entropy_bits_from_program_counter address in
  let bits = Int.min bits (width useful_address) in
  useful_address
  |> split_msb ~exact:false ~part_width:bits
  |> List.map ~f:(fun part -> bits_msb part |> shuffle |> concat_msb)
  |> List.map ~f:(Fn.flip uresize bits)
  |> tree ~arity:2 ~f:(reduce ~f:( ^: ))
;;

module Branch_target_buffer = struct
  module Entry = struct
    type 'a t =
      { target_pc : 'a [@bits Parameters.word_width]
      ; is_branch : 'a
      ; is_return : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  include
    Cache.Make
      (Entry)
      (struct
        let address_bits = Parameters.word_width
      end)

  let hierarchical =
    let open Signal in
    let size = 1024 in
    hierarchical
      ~name:"branch_target_buffer"
      ~size
      ~address_to_index:(hash_program_counter ~bits:(address_bits_for size))
      ~address_to_tag:(hash_program_counter ~bits:13)
  ;;
end

module Return_address_stack = struct
  module Overflowable_stack = struct
    module Make (Params : sig
      val size : int
    end)
    (M : Interface.S) =
    struct
      module Params = struct
        include Params

        let address_bits = Signal.address_bits_for size
      end

      module I = struct
        type 'a t =
          { clock : 'a
          ; clear : 'a
          ; push : 'a
          ; pop : 'a
          ; set_num_entries : 'a
          ; entries : 'a [@bits Params.address_bits] [@rtlprefix "restore_"]
          ; write_data : 'a M.t [@rtlprefix "wr$"]
          }
        [@@deriving sexp_of, hardcaml]
      end

      module O = struct
        type 'a t =
          { entries : 'a [@bits Params.address_bits]
          ; data : 'a M.t [@rtlprefix "rd$"]
          }
        [@@deriving sexp_of, hardcaml]
      end

      let create
        scope
        { I.clock
        ; clear
        ; push
        ; pop
        ; set_num_entries
        ; entries = restore_entries
        ; write_data
        }
        =
        assert (Params.size % 2 = 0);
        let open Signal in
        let ( -- ) = Scope.naming scope in
        let next_entries = wire Params.address_bits in
        let entries =
          (next_entries |> reg (Reg_spec.create ~clock ~clear ())) -- "entries"
        in
        next_entries
        <== mux2
              (push ^: pop |: set_num_entries)
              (mux2 push (entries +:. 1) (entries -:. 1)
               |> mux2 set_num_entries restore_entries)
              entries;
        match
          Ram.create
            ~name:(Scope.name scope "mem")
            ~collision_mode:Read_before_write
            ~size:Params.size
            ~write_ports:
              [| { write_clock = clock
                 ; write_address = mux2 pop (entries -:. 1) entries
                 ; write_enable = push
                 ; write_data = M.Of_signal.pack write_data
                 }
              |]
            ~read_ports:
              [| { read_clock = clock
                 ; read_address = mux2 pop (entries -:. 2) (entries -:. 1)
                 ; read_enable = vdd
                 }
              |]
            ()
        with
        | [| data |] ->
          { O.entries = next_entries
          ; data =
              M.Of_signal.mux2
                (push |> reg (Reg_spec.create ~clock ()))
                (M.Of_signal.reg ~enable:push (Reg_spec.create ~clock ()) write_data)
                (M.Of_signal.unpack data)
          }
        | _ -> failwith "Code out of date"
      ;;

      let hierarchical ~name scope =
        let module H = Hierarchy.In_scope (I) (O) in
        H.hierarchical ~scope ~name create
      ;;
    end

    module Tests = struct
      module Data = struct
        type 'a t = { test : 'a [@bits 4] } [@@deriving sexp_of, hardcaml]
      end

      module Params = struct
        let size = 4
      end

      open Make (Params) (Data)

      let test_bench ~f (sim : (_ I.t, _ O.t) Cyclesim.t) =
        let open Bits in
        let inputs, outputs = Cyclesim.inputs sim, Cyclesim.outputs sim in
        let print_state () =
          Stdio.print_s
            [%message
              (inputs : Bits.t ref I.t)
                (outputs : Bits.t ref O.t)
                ~internals:(Cyclesim.internal_ports sim : (string * Bits.t ref) list)];
          Stdio.print_endline "";
          ()
        in
        let clear () =
          Cyclesim.reset sim;
          inputs.clear := vdd;
          Cyclesim.cycle sim;
          inputs.clear := gnd;
          ()
        in
        clear ();
        f print_state sim;
        ()
      ;;

      let sim ~f =
        let module Simulator = Cyclesim.With_interface (I) (O) in
        let scope = Scope.create ~flatten_design:true () in
        let sim = Simulator.create ~config:Cyclesim.Config.trace_all (create scope) in
        test_bench ~f sim;
        ()
      ;;

      let%expect_test "All" =
        sim ~f:(fun print_state sim ->
          let open Bits in
          let inputs = Cyclesim.inputs sim in
          let next () =
            Cyclesim.cycle sim;
            print_state ();
            inputs.write_data.test := !(inputs.write_data.test) +:. 1;
            ()
          in
          next ();
          inputs.push := vdd;
          next ();
          inputs.push := gnd;
          next ();
          next ();
          inputs.pop := vdd;
          next ();
          inputs.pop := gnd;
          next ();
          inputs.push := vdd;
          next ();
          inputs.pop := vdd;
          next ();
          next ();
          inputs.pop := gnd;
          next ();
          next ();
          next ();
          next ();
          inputs.push := gnd;
          inputs.pop := vdd;
          next ();
          next ();
          next ();
          next ();
          next ();
          ());
        [%expect
          {|
          ((inputs
            ((clock 0) (clear 0) (push 0) (pop 0) (set_num_entries 0) (entries 00)
             (write_data ((test 0000)))))
           (outputs ((entries 00) (data ((test 0000)))))
           (internals ((mem 0000) (vdd 1) (entries_0 00))))

          ((inputs
            ((clock 0) (clear 0) (push 1) (pop 0) (set_num_entries 0) (entries 00)
             (write_data ((test 0001)))))
           (outputs ((entries 10) (data ((test 0001)))))
           (internals ((mem 0000) (vdd 1) (entries_0 00))))

          ((inputs
            ((clock 0) (clear 0) (push 0) (pop 0) (set_num_entries 0) (entries 00)
             (write_data ((test 0010)))))
           (outputs ((entries 01) (data ((test 0001)))))
           (internals ((mem 0000) (vdd 1) (entries_0 01))))

          ((inputs
            ((clock 0) (clear 0) (push 0) (pop 0) (set_num_entries 0) (entries 00)
             (write_data ((test 0011)))))
           (outputs ((entries 01) (data ((test 0001)))))
           (internals ((mem 0000) (vdd 1) (entries_0 01))))

          ((inputs
            ((clock 0) (clear 0) (push 0) (pop 1) (set_num_entries 0) (entries 00)
             (write_data ((test 0100)))))
           (outputs ((entries 11) (data ((test 0000)))))
           (internals ((mem 0000) (vdd 1) (entries_0 01))))

          ((inputs
            ((clock 0) (clear 0) (push 0) (pop 0) (set_num_entries 0) (entries 00)
             (write_data ((test 0101)))))
           (outputs ((entries 00) (data ((test 0000)))))
           (internals ((mem 0000) (vdd 1) (entries_0 00))))

          ((inputs
            ((clock 0) (clear 0) (push 1) (pop 0) (set_num_entries 0) (entries 00)
             (write_data ((test 0110)))))
           (outputs ((entries 10) (data ((test 0110)))))
           (internals ((mem 0000) (vdd 1) (entries_0 00))))

          ((inputs
            ((clock 0) (clear 0) (push 1) (pop 1) (set_num_entries 0) (entries 00)
             (write_data ((test 0111)))))
           (outputs ((entries 01) (data ((test 0111)))))
           (internals ((mem 0000) (vdd 1) (entries_0 01))))

          ((inputs
            ((clock 0) (clear 0) (push 1) (pop 1) (set_num_entries 0) (entries 00)
             (write_data ((test 1000)))))
           (outputs ((entries 01) (data ((test 1000)))))
           (internals ((mem 0000) (vdd 1) (entries_0 01))))

          ((inputs
            ((clock 0) (clear 0) (push 1) (pop 0) (set_num_entries 0) (entries 00)
             (write_data ((test 1001)))))
           (outputs ((entries 11) (data ((test 1001)))))
           (internals ((mem 0000) (vdd 1) (entries_0 01))))

          ((inputs
            ((clock 0) (clear 0) (push 1) (pop 0) (set_num_entries 0) (entries 00)
             (write_data ((test 1010)))))
           (outputs ((entries 00) (data ((test 1010)))))
           (internals ((mem 0000) (vdd 1) (entries_0 10))))

          ((inputs
            ((clock 0) (clear 0) (push 1) (pop 0) (set_num_entries 0) (entries 00)
             (write_data ((test 1011)))))
           (outputs ((entries 01) (data ((test 1011)))))
           (internals ((mem 0000) (vdd 1) (entries_0 11))))

          ((inputs
            ((clock 0) (clear 0) (push 1) (pop 0) (set_num_entries 0) (entries 00)
             (write_data ((test 1100)))))
           (outputs ((entries 10) (data ((test 1100)))))
           (internals ((mem 0000) (vdd 1) (entries_0 00))))

          ((inputs
            ((clock 0) (clear 0) (push 0) (pop 1) (set_num_entries 0) (entries 00)
             (write_data ((test 1101)))))
           (outputs ((entries 11) (data ((test 1011)))))
           (internals ((mem 0000) (vdd 1) (entries_0 01))))

          ((inputs
            ((clock 0) (clear 0) (push 0) (pop 1) (set_num_entries 0) (entries 00)
             (write_data ((test 1110)))))
           (outputs ((entries 10) (data ((test 1010)))))
           (internals ((mem 0000) (vdd 1) (entries_0 00))))

          ((inputs
            ((clock 0) (clear 0) (push 0) (pop 1) (set_num_entries 0) (entries 00)
             (write_data ((test 1111)))))
           (outputs ((entries 01) (data ((test 1001)))))
           (internals ((mem 0000) (vdd 1) (entries_0 11))))

          ((inputs
            ((clock 0) (clear 0) (push 0) (pop 1) (set_num_entries 0) (entries 00)
             (write_data ((test 0000)))))
           (outputs ((entries 00) (data ((test 1100)))))
           (internals ((mem 0000) (vdd 1) (entries_0 10))))

          ((inputs
            ((clock 0) (clear 0) (push 0) (pop 1) (set_num_entries 0) (entries 00)
             (write_data ((test 0001)))))
           (outputs ((entries 11) (data ((test 1011)))))
           (internals ((mem 0000) (vdd 1) (entries_0 01)))) |}]
      ;;
    end
  end

  module Entry = struct
    type 'a t = { return_pc : 'a [@bits Parameters.word_width] }
    [@@deriving sexp_of, hardcaml]
  end

  include
    Overflowable_stack.Make
      (struct
        let size = 32
      end)
      (Entry)

  let hierarchical = hierarchical ~name:"return_address_stack"
end

module BayesianTage = struct
  module Update = struct
    type 'a t =
      { valid : 'a
      ; resolved_direction : 'a
      ; branch_target : 'a [@bits Parameters.word_width]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Internal (Params : sig
    val num_banks : int
    val num_entries_per_bank : int
    val counter_width : int
    val tag_width : int
    val controlled_allocation_throttler_max : int
    val max_banks_skipped_on_allocation : int
    val meta_width : int
    val num_bimodal_direction_entries : int
    val num_bimodal_hysteresis_entries : int
    val bimodal_hysteresis_width : int
    val smallest_branch_history_length : int
    val largest_branch_history_length : int
    val instruction_window_size : int
    val jump_history_length : int
    val jump_history_entry_width : int
  end) =
  struct
    module Params = struct
      include Params

      let max_counter_value = Int.shift_left 1 counter_width - 1
      let bank_address_width = Signal.address_bits_for Params.num_entries_per_bank
    end

    module Bimodal_entry = struct
      type 'a t =
        { direction : 'a
        ; hysteresis : 'a [@bits Params.bimodal_hysteresis_width]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module Dual_counter = struct
      type 'a t =
        { num_takens : 'a [@bits Params.counter_width]
        ; num_not_takens : 'a [@bits Params.counter_width]
        }
      [@@deriving sexp_of, hardcaml]

      let of_bimodal { Bimodal_entry.direction; hysteresis } =
        let open Signal in
        let f direction =
          let hysteresis_max_half =
            Int.shift_left 1 (Params.bimodal_hysteresis_width - 1) - 1
          in
          mux2
            direction
            (of_int
               (hysteresis_max_half + Int.shift_left 1 Params.bimodal_hysteresis_width)
               ~width:Params.counter_width
             -: uresize hysteresis Params.counter_width)
            (of_int ~width:Params.counter_width hysteresis_max_half)
        in
        { num_takens = f direction; num_not_takens = f ~:direction }
      ;;

      (* Increments the target counter to max if possible, or decrements the opposing counter to zero. *)
      let update ~resolved_direction { num_takens; num_not_takens } =
        let open Signal in
        let next_num_takens = Always.Variable.wire ~default:num_takens in
        let next_num_not_takens = Always.Variable.wire ~default:num_not_takens in
        let counter direction = mux2 direction num_takens num_not_takens in
        Always.(
          compile
            [ if_
                (counter resolved_direction <:. Params.max_counter_value)
                [ if_
                    resolved_direction
                    [ next_num_takens <-- num_takens +:. 1 ]
                    [ next_num_not_takens <-- num_not_takens +:. 1 ]
                ]
              @@ elif
                   (counter ~:resolved_direction >:. 0)
                   [ if_
                       resolved_direction
                       [ next_num_not_takens <-- num_not_takens -:. 1 ]
                       [ next_num_takens <-- num_takens -:. 1 ]
                   ]
                   []
            ]);
        { num_takens = next_num_takens.value; num_not_takens = next_num_not_takens.value }
      ;;

      (* Decrements the leading counter. *)
      let decay { num_takens; num_not_takens } =
        let open Signal in
        let f a b = mux2 (a >: b) (a -:. 1) a in
        { num_takens = f num_takens num_not_takens
        ; num_not_takens = f num_not_takens num_takens
        }
      ;;

      let prediction { num_takens; num_not_takens } =
        let open Signal in
        num_takens >: num_not_takens
      ;;

      let is_medium_confidence { num_takens; num_not_takens } =
        let open Signal in
        let f a b = ue a ==: (b @: gnd) +:. 1 in
        f num_takens num_not_takens |: f num_not_takens num_takens
      ;;

      let is_low_confidence { num_takens; num_not_takens } =
        let open Signal in
        let f a b = ue a <: (b @: gnd) +:. 1 in
        f num_takens num_not_takens &: f num_not_takens num_takens
      ;;

      let is_high_confidence counters =
        let open Signal in
        ~:(is_medium_confidence counters) &: ~:(is_low_confidence counters)
      ;;

      let is_very_high_confidence { num_takens; num_not_takens } =
        let open Signal in
        let f a b =
          let max_half = Int.shift_left 1 (Params.counter_width - 1) in
          a ==:. 0 &: (b >=:. max_half)
        in
        f num_takens num_not_takens |: f num_not_takens num_takens
      ;;

      let sum { num_takens; num_not_takens } =
        let open Signal in
        ue num_takens +: ue num_not_takens
      ;;

      let diff { num_takens; num_not_takens } =
        let open Signal in
        mux2
          (num_takens <: num_not_takens)
          (num_not_takens -: num_takens)
          (num_takens -: num_not_takens)
      ;;

      let is_saturated { num_takens; num_not_takens } =
        let open Signal in
        let f v = v ==:. Params.max_counter_value in
        f num_takens |: f num_not_takens
      ;;

      let confidence_level ~meta ({ num_takens; num_not_takens } as counters) =
        let open Signal in
        mux2
          (num_takens ==: num_not_takens)
          (of_int ~width:2 3)
          (is_low_confidence counters
           @: (is_medium_confidence counters &: ~:(meta >=+. 0 &: (sum counters ==:. 1)))
          )
      ;;

      let select_best
        ~meta
        ~mux:mux_f
        ~f
        { With_valid.valid = older_valid; value = older }
        { With_valid.valid = newer_valid; value = newer }
        =
        let open Signal in
        { With_valid.valid = older_valid |: newer_valid
        ; value =
            mux_f
              ~:(older_valid @: newer_valid)
              [ mux_f
                  (confidence_level ~meta (f newer) <: confidence_level ~meta (f older))
                  [ older; newer ]
              ; older
              ; newer
              ]
        }
      ;;

      let select_best_single ~meta older newer =
        select_best ~meta ~mux:Of_signal.mux ~f:Fn.id older newer
      ;;
    end

    module Entry = struct
      type 'a t =
        { tag : 'a [@bits Params.tag_width]
        ; counters : 'a Dual_counter.t
        }
      [@@deriving sexp_of, hardcaml]
    end

    module Update_history = struct
      module I = struct
        type 'a t =
          { clock : 'a
          ; clear : 'a
          ; next_fetch_program_counter : 'a [@bits Parameters.word_width]
          ; retirement_program_counter : 'a [@bits Parameters.word_width]
          ; speculative_fetch_update : 'a Update.t
               [@rtlprefix "speculative_fetch_update$"]
          ; speculative_decode_update : 'a Update.t
               [@rtlprefix "speculative_decode_update$"]
          ; retirement_update : 'a Update.t [@rtlprefix "retirement_update$"]
          ; restore_from_decode : 'a
          ; restore_from_retirement : 'a
          }
        [@@deriving sexp_of, hardcaml]
      end

      module O = struct
        type 'a t =
          { prediction_indices : 'a list
               [@bits Params.bank_address_width] [@length Params.num_banks]
          ; prediction_tags : 'a list [@bits Params.tag_width] [@length Params.num_banks]
          ; retirement_indices : 'a list
               [@bits Params.bank_address_width] [@length Params.num_banks]
          ; retirement_tags : 'a list [@bits Params.tag_width] [@length Params.num_banks]
          }
        [@@deriving sexp_of, hardcaml]
      end

      module Folded_history_metadata = struct
        type t =
          { original_length : int
          ; compressed_length : int
          ; out_point : int
          ; injected_bits : int
          }
        [@@deriving sexp_of]

        let init ~original_length ~compressed_length ~injected_bits =
          { original_length
          ; compressed_length
          ; out_point = original_length % compressed_length
          ; injected_bits = Int.min injected_bits compressed_length
          }
        ;;
      end

      module Indices_and_tags = struct
        type 'a branch_history =
          { index : 'a
          ; tag : 'a
          }
        [@@deriving sexp_of]

        type 'a jump_history =
          { index : 'a
          ; tag : 'a
          }
        [@@deriving sexp_of]

        type 'a t =
          { branch : 'a branch_history
          ; jump : 'a jump_history
          }
        [@@deriving sexp_of]

        let map
          { branch = { index = branch_index; tag = branch_tag }
          ; jump = { index = jump_index; tag = jump_tag }
          }
          ~f
          =
          { branch = { index = f branch_index; tag = f branch_tag }
          ; jump = { index = f jump_index; tag = f jump_tag }
          }
        ;;

        let transpose
          ( { branch = { index = branch_index1; tag = branch_tag1 }
            ; jump = { index = jump_index1; tag = jump_tag1 }
            }
          , { branch = { index = branch_index2; tag = branch_tag2 }
            ; jump = { index = jump_index2; tag = jump_tag2 }
            } )
          =
          { branch =
              { index = branch_index1, branch_index2; tag = branch_tag1, branch_tag2 }
          ; jump = { index = jump_index1, jump_index2; tag = jump_tag1, jump_tag2 }
          }
        ;;
      end

      let folded_histories =
        let history_lengths =
          List.init Params.num_banks ~f:(fun i ->
            Float.(
              of_int Params.smallest_branch_history_length
              *. ((of_int Params.largest_branch_history_length
                   /. of_int Params.smallest_branch_history_length)
                  ** (of_int i /. of_int Int.(Params.num_banks - 1)))
              |> to_int))
          |> List.folding_map ~init:0 ~f:(fun prev_history_length history_length ->
               let h = Int.max (prev_history_length + 1) history_length in
               h, h)
          |> List.rev
        in
        List.map history_lengths ~f:(fun history_length ->
          { Indices_and_tags.branch =
              (let init =
                 Folded_history_metadata.init
                   ~original_length:history_length
                   ~injected_bits:1
               in
               { index = init ~compressed_length:Params.bank_address_width
               ; tag = init ~compressed_length:Params.tag_width
               })
          ; jump =
              (let init =
                 Folded_history_metadata.init
                   ~original_length:(Int.min history_length Params.jump_history_length)
                   ~injected_bits:
                     (if history_length < Params.jump_history_length
                      then Params.jump_history_entry_width
                      else 1)
               in
               { index =
                   (let hash_param =
                      if Params.bank_address_width = Params.tag_width
                      then (
                        let lcm m n =
                          let rec gcd u v = if v <> 0 then gcd v (u mod v) else abs u in
                          match m, n with
                          | 0, _ | _, 0 -> 0
                          | m, n -> abs (m * n) / gcd m n
                        in
                        if lcm Params.bank_address_width (Params.bank_address_width - 3)
                           > lcm Params.bank_address_width (Params.bank_address_width - 2)
                        then 3
                        else 2)
                      else 1
                    in
                    init ~compressed_length:(Params.bank_address_width - hash_param))
               ; tag = init ~compressed_length:(Params.tag_width - 1)
               })
          })
      ;;

      let create
        scope
        ~hash
        { I.clock
        ; clear
        ; next_fetch_program_counter
        ; retirement_program_counter
        ; speculative_fetch_update =
            { valid = speculative_fetch_valid
            ; resolved_direction = speculative_fetch_resolved_direction
            ; branch_target = speculative_fetch_branch_target
            }
        ; speculative_decode_update =
            { valid = speculative_decode_valid
            ; resolved_direction = speculative_decode_resolved_direction
            ; branch_target = speculative_decode_branch_target
            }
        ; retirement_update =
            { valid = retirement_valid
            ; resolved_direction = retirement_resolved_direction
            ; branch_target = retirement_branch_target
            }
        ; restore_from_decode
        ; restore_from_retirement
        }
        =
        let open Signal in
        let ( -- ) = Scope.naming scope in
        let next_fetch_tag, retirement_tag =
          (next_fetch_program_counter, retirement_program_counter)
          |> Tuple2.map ~f:(hash ~bits:Params.tag_width)
        in
        let next_fetch_program_counter, retirement_program_counter =
          (next_fetch_program_counter, retirement_program_counter)
          |> Tuple2.map ~f:(hash ~bits:Params.bank_address_width)
        in
        let ( speculative_fetch_branch_target
            , speculative_decode_branch_target
            , retirement_branch_target )
          =
          ( speculative_fetch_branch_target
          , speculative_decode_branch_target
          , retirement_branch_target )
          |> Tuple3.map ~f:(hash ~bits:Params.jump_history_entry_width)
        in
        let () =
          next_fetch_tag -- "next_fetch_tag" |> ignore;
          retirement_tag -- "retirement_tag" |> ignore;
          next_fetch_program_counter -- "next_fetch_program_counter" |> ignore;
          retirement_program_counter -- "retirement_program_counter" |> ignore;
          speculative_fetch_branch_target -- "speculative_fetch_branch_target" |> ignore;
          speculative_decode_branch_target -- "speculative_decode_branch_target" |> ignore;
          retirement_branch_target -- "retirement_branch_target" |> ignore;
          ()
        in
        let gen_path_history ~name ~length ~entry_width ~inputs =
          let length = length + Params.instruction_window_size in
          let pointers =
            let width = address_bits_for length in
            let gen_pointer ~enable ~restore ~restore_from =
              reg_fb
                ~width
                ~f:(fun pointer ->
                  let pointer = mux2 restore restore_from pointer in
                  let next_pointer =
                    mux2 (pointer ==:. 0) (of_int ~width (length - 1)) (pointer -:. 1)
                  in
                  mux2 enable next_pointer pointer)
                (Reg_spec.create ~clock ~clear ())
            in
            let retirement_pointer =
              gen_pointer ~enable:retirement_valid ~restore:gnd ~restore_from:(zero width)
            in
            let decode_pointer =
              gen_pointer
                ~enable:speculative_decode_valid
                ~restore:restore_from_retirement
                ~restore_from:retirement_pointer
            in
            let fetch_pointer =
              gen_pointer
                ~enable:speculative_fetch_valid
                ~restore:(restore_from_decode |: restore_from_retirement)
                ~restore_from:
                  (mux2 restore_from_retirement retirement_pointer decode_pointer)
            in
            let () =
              fetch_pointer -- Printf.sprintf "fetch$%s_pointer" name |> ignore;
              decode_pointer -- Printf.sprintf "decode$%s_pointer" name |> ignore;
              retirement_pointer -- Printf.sprintf "retirement$%s_pointer" name |> ignore;
              ()
            in
            [ fetch_pointer; decode_pointer; retirement_pointer ]
          in
          let inputs_with_valid =
            List.zip_exn
              inputs
              [ speculative_fetch_valid; speculative_decode_valid; retirement_valid ]
            |> List.map ~f:(fun (input, valid) -> { With_valid.valid; value = input })
          in
          ( pointers
          , List.map pointers ~f:binary_to_onehot
            |> List.map ~f:bits_lsb
            |> List.transpose_exn
            |> List.map ~f:(Fn.flip List.zip_exn inputs_with_valid)
            |> List.map
                 ~f:
                   (List.map ~f:(fun (update, { With_valid.valid; value = input }) ->
                      { With_valid.valid = update &: valid; value = input }))
            |> Fn.flip List.take length
            |> List.map ~f:(fun updates ->
                 reg_fb
                   ~width:entry_width
                   ~f:(fun entry ->
                     List.rev updates |> priority_select_with_default ~default:entry)
                   (Reg_spec.create ~clock ())) )
        in
        let branch_pointers, branch_history =
          gen_path_history
            ~name:"branch_direction"
            ~length:Params.largest_branch_history_length
            ~entry_width:1
            ~inputs:
              [ speculative_fetch_resolved_direction
              ; speculative_decode_resolved_direction
              ; retirement_resolved_direction
              ]
        in
        let jump_pointers, jump_history =
          gen_path_history
            ~name:"jump_target"
            ~length:Params.jump_history_length
            ~entry_width:Params.jump_history_entry_width
            ~inputs:
              [ speculative_fetch_branch_target
              ; speculative_decode_branch_target
              ; retirement_branch_target
              ]
        in
        let () =
          List.mapi branch_history ~f:(fun i s ->
            s -- Printf.sprintf "branch_history_%d" i)
          |> ignore;
          List.mapi jump_history ~f:(fun i s -> s -- Printf.sprintf "jump_history_%d" i)
          |> ignore;
          ()
        in
        let (prediction_indices, prediction_tags), (retirement_indices, retirement_tags) =
          let indices_and_tags
            ~name
            ~enable
            ~restore
            ~restore_from
            ~head_entry:(head_branch_entry, head_jump_entry)
            ~branch_pointer
            ~jump_pointer
            =
            let update
              ~name
              ~restore_from
              ~head_entry
              ~history
              ~pointer
              { Folded_history_metadata.original_length
              ; compressed_length
              ; out_point
              ; injected_bits
              }
              =
              let next_folded_history = wire compressed_length in
              let folded_history =
                next_folded_history |> reg (Reg_spec.create ~clock ~clear ())
              in
              (next_folded_history
               <==
               let folded_history = mux2 restore restore_from folded_history in
               let next =
                 let head_entry =
                   sel_bottom head_entry injected_bits
                   |> Fn.flip uresize compressed_length
                 in
                 let tail_entry =
                   let tail = ue pointer +:. (original_length - 1) in
                   let tail =
                     mux2
                       (tail >=:. List.length history)
                       (tail -:. List.length history)
                       tail
                     |> lsbs
                   in
                   mux tail history
                   |> Fn.flip sel_bottom injected_bits
                   |> Fn.flip uresize compressed_length
                 in
                 let () =
                   head_entry -- Printf.sprintf "%s_head_entry" name |> ignore;
                   tail_entry -- Printf.sprintf "%s_tail_entry" name |> ignore;
                   folded_history -- Printf.sprintf "%s_folded_history" name |> ignore;
                   ()
                 in
                 head_entry ^: rotl folded_history 1 ^: rotl tail_entry out_point
               in
               mux2 enable next folded_history);
              folded_history, next_folded_history
            in
            List.zip_exn folded_histories restore_from
            |> List.mapi
                 ~f:(fun
                      i
                      ( { Indices_and_tags.branch =
                            { index = branch_index; tag = branch_tag }
                        ; jump = { index = jump_index; tag = jump_tag }
                        }
                      , { Indices_and_tags.branch =
                            { index = restore_branch_index; tag = restore_branch_tag }
                        ; jump = { index = restore_jump_index; tag = restore_jump_tag }
                        } )
                    ->
                 let update ~type_name =
                   update ~name:(Printf.sprintf "%s%s_%d" name type_name i)
                 in
                 { Indices_and_tags.branch =
                     (let update =
                        update
                          ~head_entry:head_branch_entry
                          ~history:branch_history
                          ~pointer:branch_pointer
                      in
                      { index =
                          update
                            ~type_name:"branch_index"
                            ~restore_from:restore_branch_index
                            branch_index
                      ; tag =
                          update
                            ~type_name:"jump_index"
                            ~restore_from:restore_branch_tag
                            branch_tag
                      })
                 ; jump =
                     (let update =
                        update
                          ~head_entry:head_jump_entry
                          ~history:jump_history
                          ~pointer:jump_pointer
                      in
                      { index =
                          update
                            ~type_name:"jump_index"
                            ~restore_from:restore_jump_index
                            jump_index
                      ; tag =
                          update
                            ~type_name:"jump_tag"
                            ~restore_from:restore_jump_tag
                            jump_tag
                      })
                 })
          in
          let extract_pointers = function
            | [ fetch; decode; retirement ] -> fetch, decode, retirement
            | _ -> failwith "Code out of date"
          in
          let branch_fetch_pointer, branch_decode_pointer, branch_retirement_pointer =
            extract_pointers branch_pointers
          in
          let jump_fetch_pointer, jump_decode_pointer, jump_retirement_pointer =
            extract_pointers jump_pointers
          in
          let retirement_indices_and_tags =
            indices_and_tags
              ~name:"retirement$entries$"
              ~enable:retirement_valid
              ~restore:gnd
              ~restore_from:
                (List.map
                   folded_histories
                   ~f:
                     (Indices_and_tags.map
                        ~f:(fun { Folded_history_metadata.compressed_length; _ } ->
                        zero compressed_length)))
              ~head_entry:(retirement_resolved_direction, retirement_branch_target)
              ~branch_pointer:branch_retirement_pointer
              ~jump_pointer:jump_retirement_pointer
          in
          let decode_indices_and_tags =
            indices_and_tags
              ~name:"decode$entries$"
              ~enable:speculative_decode_valid
              ~restore:restore_from_retirement
              ~restore_from:
                (List.map retirement_indices_and_tags ~f:(Indices_and_tags.map ~f:fst))
              ~head_entry:
                (speculative_decode_resolved_direction, speculative_decode_branch_target)
              ~branch_pointer:branch_decode_pointer
              ~jump_pointer:jump_decode_pointer
          in
          let fetch_indices_and_tags =
            indices_and_tags
              ~name:"fetch$entries$"
              ~enable:speculative_fetch_valid
              ~restore:(restore_from_decode |: restore_from_retirement)
              ~restore_from:
                (List.zip_exn decode_indices_and_tags retirement_indices_and_tags
                 |> List.map ~f:(Tuple2.map ~f:(Indices_and_tags.map ~f:fst))
                 |> List.map ~f:Indices_and_tags.transpose
                 |> List.map
                      ~f:
                        (Indices_and_tags.map ~f:(fun (decode, retirement) ->
                           mux2 restore_from_retirement retirement decode)))
              ~head_entry:
                (speculative_fetch_resolved_direction, speculative_fetch_branch_target)
              ~branch_pointer:branch_fetch_pointer
              ~jump_pointer:jump_fetch_pointer
          in
          List.zip_exn fetch_indices_and_tags retirement_indices_and_tags
          |> List.map ~f:(Tuple2.map ~f:(Indices_and_tags.map ~f:snd))
          |> List.mapi
               ~f:(fun
                    i
                    ( { branch = { index = fetch_branch_index; tag = fetch_branch_tag }
                      ; jump = { index = fetch_jump_index; tag = fetch_jump_tag }
                      }
                    , { branch =
                          { index = retirement_branch_index; tag = retirement_branch_tag }
                      ; jump =
                          { index = retirement_jump_index; tag = retirement_jump_tag }
                      } )
                  ->
               let indices ~pc ~branch ~jump =
                 pc ^: branch ^: (jump @: zero (width branch - width jump)) ^:. i
               in
               let tags ~pc ~branch ~jump =
                 (pc +:. i) ^: reverse branch ^: jump @: zero (width branch - width jump)
               in
               ( ( indices
                     ~pc:next_fetch_program_counter
                     ~branch:fetch_branch_index
                     ~jump:fetch_jump_index
                 , tags ~pc:next_fetch_tag ~branch:fetch_branch_tag ~jump:fetch_jump_tag
                 )
               , ( indices
                     ~pc:retirement_program_counter
                     ~branch:retirement_branch_index
                     ~jump:retirement_jump_index
                 , tags
                     ~pc:retirement_tag
                     ~branch:retirement_branch_tag
                     ~jump:retirement_jump_tag ) ))
          |> List.unzip
          |> Tuple2.map ~f:List.unzip
        in
        { O.prediction_indices; prediction_tags; retirement_indices; retirement_tags }
      ;;

      let hierarchical scope =
        let module H = Hierarchy.In_scope (I) (O) in
        H.hierarchical ~scope ~name:"update_history" (create ~hash:hash_program_counter)
      ;;
    end

    module Update_counters = struct
      module I = struct
        type 'a t =
          { clock : 'a
          ; clear : 'a
          ; update : 'a Update.t
          ; read_entries : 'a Entry.t list
               [@length Params.num_banks] [@rtlprefix "read_entries$"]
          ; tags : 'a list [@bits Params.tag_width] [@length Params.num_banks]
          ; bimodal_entry : 'a Bimodal_entry.t [@rtlprefix "bimodal_entry$"]
          }
        [@@deriving sexp_of, hardcaml]
      end

      module O = struct
        type 'a t =
          { next_entries : 'a Entry.t list
               [@length Params.num_banks] [@rtlprefix "next_entries$"]
          ; next_bimodal_entry : 'a Bimodal_entry.t [@rtlprefix "next_bimodal_entry$"]
          ; meta : 'a [@bits Params.meta_width]
          }
        [@@deriving sexp_of, hardcaml]
      end

      let randoms ~clock ~clear ~enable =
        let open Signal in
        let gen_random x =
          let x = x ^: sll x 13 in
          let x = x ^: srl x 17 in
          let x = x ^: sll x 5 in
          x
        in
        let rng ~start =
          let width = 32 in
          reg_fb
            ~enable
            ~width
            ~f:gen_random
            (Reg_spec.create ~clock ~clear ()
             |> Reg_spec.override ~clear_to:(of_int ~width start))
        in
        ( rng ~start:2463534242
        , rng ~start:1850600128
        , rng ~start:3837179466
        , rng ~start:4290344314
        , rng ~start:0614373416 )
      ;;

      (* TODO optimize out bank comparators *)
      let create
        scope
        { I.clock
        ; clear
        ; update = { valid; resolved_direction; branch_target = _ }
        ; read_entries
        ; tags
        ; bimodal_entry
        }
        =
        let open Signal in
        let ( -- ) = Scope.naming scope in
        let bank_num_width = address_bits_for (Params.num_banks + 1) in
        let base_prediction_counters = Dual_counter.of_bimodal bimodal_entry in
        let hit_vector =
          List.map2_exn
            read_entries
            tags
            ~f:(fun { Entry.tag = entry_tag; counters = _ } tag -> entry_tag ==: tag)
        in
        concat_msb hit_vector -- "hit_vector" |> ignore;
        let any_hitters = tree ~arity:2 hit_vector ~f:(reduce ~f:( |: )) in
        let module Counters_and_metadata = struct
          type 'a t =
            { counters : 'a Dual_counter.t
            ; bank : 'a [@bits bank_num_width]
            ; mask : 'a [@bits Params.num_banks]
            }
          [@@deriving sexp_of, hardcaml]

          let of_bank_and_counters ~f bank counters =
            { counters
            ; bank = of_int ~width:bank_num_width bank
            ; mask = String.init Params.num_banks ~f |> of_bit_string
            }
          ;;

          let for_hitters bank =
            of_bank_and_counters bank ~f:(fun i -> if i = bank then '0' else '1')
          ;;

          let for_prediction bank =
            of_bank_and_counters bank ~f:(fun i -> if i <= bank then '0' else '1')
          ;;
        end
        in
        let select_oldest_hitter ?(f = Counters_and_metadata.for_hitters) hit_vector =
          List.zip_exn hit_vector read_entries
          |> List.mapi ~f:(fun bank (hit, { Entry.tag = _; counters }) ->
               { With_valid.valid = hit
               ; value = f bank counters |> Counters_and_metadata.Of_signal.pack
               })
          |> priority_select_with_default
               ~default:
                 (f Params.num_banks base_prediction_counters
                  |> Counters_and_metadata.Of_signal.pack)
          |> Counters_and_metadata.Of_signal.unpack
        in
        let { Counters_and_metadata.counters = first_hitter_counters
            ; bank = first_hitter_bank
            ; mask = first_hitter_mask
            }
          =
          select_oldest_hitter hit_vector
        in
        first_hitter_bank -- "first_hitter_bank" |> ignore;
        first_hitter_mask -- "first_hitter_mask" |> ignore;
        let meta, next_meta =
          let { Counters_and_metadata.counters = second_hitter_counters
              ; bank = _
              ; mask = _
              }
            =
            first_hitter_mask &: concat_msb hit_vector |> bits_msb |> select_oldest_hitter
          in
          let next_meta = wire Params.meta_width -- "next_meta" in
          let meta = next_meta |> reg ~enable:valid (Reg_spec.create ~clock ~clear ()) in
          let () =
            let next_meta_var = Always.Variable.wire ~default:meta in
            Always.(
              compile
                [ when_
                    ([ any_hitters
                     ; Dual_counter.sum first_hitter_counters ==:. 1
                     ; Dual_counter.is_high_confidence second_hitter_counters
                     ; Dual_counter.prediction first_hitter_counters
                       <>: Dual_counter.prediction second_hitter_counters
                     ]
                     |> tree ~arity:2 ~f:(reduce ~f:( &: )))
                    [ (let meta_max = Int.shift_left 1 (Params.meta_width - 1) - 1 in
                       let meta_min = -meta_max - 1 in
                       if_
                         (Dual_counter.prediction first_hitter_counters
                          ==: resolved_direction)
                         [ when_ (meta <+. meta_max) [ next_meta_var <-- meta +:. 1 ] ]
                         [ when_ (meta >+. meta_min) [ next_meta_var <-- meta -:. 1 ] ])
                    ]
                ]);
            next_meta <== next_meta_var.value;
            ()
          in
          meta, next_meta
        in
        let { Counters_and_metadata.counters = prediction_counters
            ; bank = bank_used_for_prediction
            ; mask = prediction_mask
            }
          =
          let older =
            List.zip_exn hit_vector read_entries
            |> List.mapi ~f:(fun bank (hit, { Entry.tag = _; counters }) ->
                 { With_valid.valid = hit
                 ; value = Counters_and_metadata.for_prediction bank counters
                 })
            |> tree
                 ~arity:2
                 ~f:
                   (reduce
                      ~f:
                        (Dual_counter.select_best
                           ~meta:next_meta
                           ~mux:Counters_and_metadata.Of_signal.mux
                           ~f:(fun cm -> cm.counters)))
          in
          (Dual_counter.select_best
             ~meta
             ~mux:Counters_and_metadata.Of_signal.mux
             ~f:(fun cm -> cm.counters)
             older
             { With_valid.valid = vdd
             ; value =
                 Counters_and_metadata.for_prediction
                   Params.num_banks
                   base_prediction_counters
             })
            .value
        in
        bank_used_for_prediction -- "bank_used_for_prediction" |> ignore;
        let { Counters_and_metadata.counters = hitter_after_prediction_counters
            ; bank = hitter_after_prediction_bank
            ; mask = _
            }
          =
          prediction_mask &: concat_msb hit_vector |> bits_msb |> select_oldest_hitter
        in
        hitter_after_prediction_bank -- "hitter_after_prediction_bank" |> ignore;
        let random1, random2, random3, random4, random5 =
          randoms ~clock ~clear ~enable:valid
        in
        let () =
          (* TODO these randoms are used incorrectly.
             They should be generated repeated in the loops but currently aren't.
             We should look at the bit widths needed and generate enough randoms to all possible random usage.
          *)
          random1 -- "randoms$1" |> ignore;
          random2 -- "randoms$2" |> ignore;
          random3 -- "randoms$3" |> ignore;
          random4 -- "randoms$4" |> ignore;
          random5 -- "randoms$5" |> ignore;
          ()
        in
        let next_controlled_allocation_throttler =
          wire (Int.floor_log2 (Params.controlled_allocation_throttler_max + 1))
          -- "next_controlled_allocation_throttler"
        in
        let controlled_allocation_throttler =
          (next_controlled_allocation_throttler
           |> reg ~enable:valid (Reg_spec.create ~clock ~clear ()))
          -- "controlled_allocation_throttler"
        in
        let { With_valid.valid = end_allocation_bank_valid; value = end_allocation_bank } =
          let skip =
            let mods =
              List.init (Params.max_counter_value + 1) ~f:(fun diff ->
                1
                + (diff
                   * Params.max_banks_skipped_on_allocation
                   / Params.max_counter_value))
            in
            List.map mods ~f:(fun mod_ ->
              let skip =
                if mod_ = 1 then zero 1 else sel_bottom random4 (Int.ceil_log2 mod_)
              in
              mux2 (skip >=:. mod_) (skip -:. mod_) skip
              |> Fn.flip uresize Params.counter_width)
            |> mux (Dual_counter.diff first_hitter_counters)
            |> Fn.flip uresize bank_num_width
          in
          skip -- "skipped_allocation_banks" |> ignore;
          { With_valid.valid = skip <: first_hitter_bank
          ; value = first_hitter_bank -: skip
          }
        in
        let end_allocation_bank_mask =
          List.init (Params.num_banks + 1) ~f:(fun banks ->
            String.init (Params.num_banks + 1) ~f:(fun bank ->
              if bank < banks then '1' else '0'))
          |> List.map ~f:of_bit_string
          |> mux end_allocation_bank
          |> msbs
        in
        end_allocation_bank_valid -- "end_allocation_bank_valid" |> ignore;
        end_allocation_bank -- "end_allocation_bank" |> ignore;
        end_allocation_bank_mask -- "end_allocation_bank_mask" |> ignore;
        let { Counters_and_metadata.counters = _; bank = allocation_bank; mask = _ } =
          List.map2_exn
            (bits_msb end_allocation_bank_mask)
            read_entries
            ~f:(fun active { Entry.tag = _; counters } ->
            active &: ~:(Dual_counter.is_high_confidence counters))
          |> List.rev
          |> select_oldest_hitter ~f:(fun bank ->
               Counters_and_metadata.for_hitters (Params.num_banks - (bank + 1)))
        in
        let allocation_bank_mask =
          List.init (Params.num_banks + 1) ~f:(fun banks ->
            String.init (Params.num_banks + 1) ~f:(fun bank ->
              if bank > banks || banks = Params.num_banks then '1' else '0'))
          |> List.map ~f:of_bit_string
          |> mux allocation_bank
          |> msbs
        in
        allocation_bank -- "allocation_bank" |> ignore;
        allocation_bank_mask -- "allocation_bank_mask" |> ignore;
        let allocation_decay_range = allocation_bank_mask &: end_allocation_bank_mask in
        allocation_decay_range -- "allocation_decay_range" |> ignore;
        let allocate =
          [ allocation_bank <>:. Params.num_banks
          ; end_allocation_bank_valid
          ; Dual_counter.prediction prediction_counters <>: resolved_direction
          ; ((* TODO I don't know what this stands for. A is probably allocation. *)
             let min_ap = 4 in
             uresize (sel_bottom random3 min_ap) (width controlled_allocation_throttler)
             >=: srl
                   controlled_allocation_throttler
                   (Int.floor_log2 (Params.controlled_allocation_throttler_max + 1)
                    - min_ap))
          ]
          |> tree ~arity:2 ~f:(reduce ~f:( &: ))
        in
        allocate -- "allocate" |> ignore;
        (* TODO implement cd *)
        (next_controlled_allocation_throttler
         <==
         let max_range = Int.ceil_log2 (Params.max_banks_skipped_on_allocation + 1) in
         let mhc =
           List.map2_exn
             (bits_msb allocation_decay_range)
             read_entries
             ~f:(fun active { Entry.tag = _; counters } ->
             mux2
               (active &: ~:(Dual_counter.is_very_high_confidence counters))
               (one max_range)
               (zero max_range))
           |> tree ~arity:2 ~f:(reduce ~f:( +: ))
         in
         mhc -- "mhc" |> ignore;
         (* TODO don't know what the R or DEN stands for.
            CATR_NUM - mhc * CATR_DEN
          *)
         let catr_num, _catr_den = 2, 3 in
         let width = width controlled_allocation_throttler in
         let diff =
           of_int ~width:(max_range + 2) catr_num
           -: (ue (mhc @: gnd) +: uresize mhc (max_range + 2))
           |> Fn.flip sresize width
         in
         let result =
           uresize controlled_allocation_throttler (width + 2) +: sresize diff (width + 2)
         in
         let next =
           mux2
             (result >+. Params.controlled_allocation_throttler_max)
             (of_int ~width Params.controlled_allocation_throttler_max)
             (mux2 (result <+. 0) (zero width) (uresize result width))
         in
         mux2 allocate next controlled_allocation_throttler);
        { O.next_entries =
            List.zip_exn hit_vector read_entries
            |> List.zip_exn tags
            |> List.mapi ~f:(fun bank (next_tag, (hit, { tag; counters })) ->
                 { Entry.tag = mux2 (allocate &: (allocation_bank ==:. bank)) next_tag tag
                 ; counters =
                     (let open Dual_counter in
                      let next_counters = Of_always.wire zero in
                      Always.(
                        let maybe_update_oldest_hitters =
                          when_
                            ([ next_meta >=+. 0
                             ; is_low_confidence counters
                             ; prediction counters <>: prediction prediction_counters
                             ; sel_bottom random1 3 ==:. 0
                             ]
                             |> tree ~arity:2 ~f:(reduce ~f:( |: )))
                            [ update ~resolved_direction counters
                              |> Of_always.assign next_counters
                            ]
                        in
                        let maybe_update_hitter =
                          if_
                            ([ is_high_confidence counters
                             ; is_high_confidence hitter_after_prediction_counters
                             ; prediction hitter_after_prediction_counters
                               ==: resolved_direction
                             ; prediction counters
                               ==: resolved_direction
                               |: (controlled_allocation_throttler
                                   >=:. Params.controlled_allocation_throttler_max / 2)
                             ]
                             |> tree ~arity:2 ~f:(reduce ~f:( &: )))
                            [ when_
                                (~:(is_saturated counters)
                                 |: (next_meta <+. 0 &: (sel_bottom random2 8 ==:. 0)))
                                [ decay counters |> Of_always.assign next_counters ]
                            ]
                            [ update ~resolved_direction counters
                              |> Of_always.assign next_counters
                            ]
                        in
                        let maybe_update_younger_hitter =
                          when_
                            ~:(is_high_confidence prediction_counters)
                            [ update ~resolved_direction counters
                              |> Of_always.assign next_counters
                            ]
                        in
                        let maybe_decay_on_allocation =
                          when_
                            (is_high_confidence prediction_counters
                             &: (sel_bottom random5 2 ==:. 0))
                            [ decay counters |> Of_always.assign next_counters ]
                        in
                        compile
                          [ Of_always.assign next_counters counters
                          ; when_
                              hit
                              [ when_
                                  (bank_used_for_prediction >:. bank)
                                  [ maybe_update_oldest_hitters ]
                              ; when_
                                  (bank_used_for_prediction ==:. bank)
                                  [ maybe_update_hitter ]
                              ; when_
                                  (hitter_after_prediction_bank ==:. bank)
                                  [ maybe_update_younger_hitter ]
                              ]
                          ; when_
                              (allocate -- "zzreallyallocate")
                              [ when_
                                  (allocation_bank
                                   <:. bank
                                   &: (end_allocation_bank >:. bank))
                                  [ maybe_decay_on_allocation ]
                              ; when_
                                  ((allocation_bank ==:. bank)
                                   -- Printf.sprintf "zzbank-eq%d" bank)
                                  [ (let result =
                                       Of_signal.of_int 0 |> update ~resolved_direction
                                     in
                                     result.num_takens -- Printf.sprintf "zztakens%d" bank
                                     |> ignore;
                                     result.num_not_takens
                                     -- Printf.sprintf "zznottakens%d" bank
                                     |> ignore;
                                     result |> Of_always.assign next_counters)
                                  ]
                              ]
                          ]);
                      Of_always.value next_counters)
                 })
        ; next_bimodal_entry =
            (let ({ Bimodal_entry.direction = next_direction
                  ; hysteresis = next_hysteresis
                  } as next_bimodal)
               =
               Bimodal_entry.Of_always.wire zero
             in
             Always.(
               let maybe_update_bimodal =
                 let { Bimodal_entry.direction; hysteresis } = bimodal_entry in
                 if_
                   (direction ==: resolved_direction)
                   [ (let hysteresis_max =
                        Int.shift_left 1 Params.bimodal_hysteresis_width - 1
                      in
                      when_
                        (hysteresis <:. hysteresis_max)
                        [ next_hysteresis <-- hysteresis +:. 1 ])
                   ]
                 @@ elif
                      (hysteresis >:. 0)
                      [ next_hysteresis <-- hysteresis -:. 1 ]
                      [ next_direction <-- resolved_direction ]
               in
               compile
                 [ Bimodal_entry.Of_always.assign next_bimodal bimodal_entry
                 ; when_
                     (bank_used_for_prediction
                      ==:. Params.num_banks
                      |: (hitter_after_prediction_bank
                          ==:. Params.num_banks
                          &: ~:(Dual_counter.is_high_confidence prediction_counters)))
                     [ maybe_update_bimodal ]
                 ]);
             Bimodal_entry.Of_always.value next_bimodal)
        ; meta
        }
      ;;

      let hierarchical scope =
        let module H = Hierarchy.In_scope (I) (O) in
        H.hierarchical ~scope ~name:"update_counters" create
      ;;
    end

    module Predict = struct
      module I = struct
        type 'a t =
          { read_entries : 'a Entry.t list [@length Params.num_banks]
          ; tags : 'a list [@bits Params.tag_width] [@length Params.num_banks]
          ; bimodal_entry : 'a Bimodal_entry.t
          ; meta : 'a [@bits Params.meta_width]
          }
        [@@deriving sexp_of, hardcaml]
      end

      module O = struct
        type 'a t = { taken : 'a } [@@deriving sexp_of, hardcaml]
      end

      let create _scope { I.read_entries; tags; bimodal_entry; meta } =
        let open Signal in
        let highest_confidence_counter =
          let older =
            List.zip_exn read_entries tags
            |> List.map ~f:(fun ({ Entry.tag = entry_tag; counters }, tag) ->
                 { With_valid.valid = tag ==: entry_tag; value = counters })
            |> tree ~arity:2 ~f:(reduce ~f:(Dual_counter.select_best_single ~meta))
          in
          (Dual_counter.select_best_single
             ~meta
             older
             { With_valid.valid = vdd; value = Dual_counter.of_bimodal bimodal_entry })
            .value
        in
        { O.taken = Dual_counter.prediction highest_confidence_counter }
      ;;

      let hierarchical scope =
        let module H = Hierarchy.In_scope (I) (O) in
        H.hierarchical ~scope ~name:"predict" create
      ;;
    end
  end

  module Params = struct
    type t =
      { num_banks : int
      ; num_entries_per_bank : int
      ; counter_width : int
      ; tag_width : int
      ; controlled_allocation_throttler_max : int
      ; max_banks_skipped_on_allocation : int
      ; meta_width : int
      ; num_bimodal_direction_entries : int
      ; num_bimodal_hysteresis_entries : int
      ; bimodal_hysteresis_width : int
      ; smallest_branch_history_length : int
      ; largest_branch_history_length : int
      ; instruction_window_size : int
      ; jump_history_length : int
      ; jump_history_entry_width : int
      }
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; next_fetch_program_counter : 'a [@bits Parameters.word_width]
      ; retirement_program_counter : 'a [@bits Parameters.word_width]
      ; speculative_fetch_update : 'a Update.t [@rtlprefix "speculative_fetch_update$"]
      ; speculative_decode_update : 'a Update.t [@rtlprefix "speculative_decode_update$"]
      ; retirement_update : 'a Update.t [@rtlprefix "retirement_update$"]
      ; restore_from_decode : 'a
      ; restore_from_retirement : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { predicted_direction : 'a } [@@deriving sexp_of, hardcaml]
  end

  let forwarding_ram scope ~clock ~name ~collision_mode ~size ~write_ports ~read_ports () =
    let open Signal in
    let ( -- ) = Scope.naming scope in
    Array.iteri read_ports ~f:(fun i { read_clock = _; read_address; read_enable } ->
      read_address -- Printf.sprintf "%s$read_address%d" name i |> ignore;
      read_enable -- Printf.sprintf "%s$read_enable%d" name i |> ignore;
      ());
    Array.iteri
      write_ports
      ~f:(fun i { write_clock = _; write_address; write_enable; write_data } ->
      write_address -- Printf.sprintf "%s$write_address%d" name i |> ignore;
      write_enable -- Printf.sprintf "%s$write_enable%d" name i |> ignore;
      write_data -- Printf.sprintf "%s$write_data%d" name i |> ignore;
      ());
    let writes =
      Array.map
        write_ports
        ~f:(fun { write_clock = _; write_address; write_enable; write_data } ->
        write_address @: write_data
        |> reg ~enable:write_enable (Reg_spec.create ~clock ()))
    in
    let outputs =
      Ram.create
        ~name:(Scope.name scope name)
        ~collision_mode
        ~size
        ~write_ports
        ~read_ports
        ()
      |> Array.zip_exn read_ports
      |> Array.map ~f:(fun ({ read_clock = _; read_address; read_enable }, read_output) ->
           let read_address =
             read_address |> reg ~enable:read_enable (Reg_spec.create ~clock ())
           in
           Array.to_list writes
           |> List.map ~f:(fun forwarded ->
                let forwarded_address = sel_top forwarded (width read_address) in
                let forwarded_data = drop_top forwarded (width read_address) in
                { With_valid.valid = forwarded_address ==: read_address
                ; value = forwarded_data
                })
           |> priority_select_with_default ~default:read_output)
    in
    Array.iteri outputs ~f:(fun i data ->
      data -- Printf.sprintf "%s$data%d" name i |> ignore;
      ());
    outputs
  ;;

  let create
    scope
    ~params:
      { Params.num_banks
      ; num_entries_per_bank
      ; counter_width
      ; tag_width
      ; controlled_allocation_throttler_max
      ; max_banks_skipped_on_allocation
      ; meta_width
      ; num_bimodal_direction_entries
      ; num_bimodal_hysteresis_entries
      ; bimodal_hysteresis_width
      ; smallest_branch_history_length
      ; largest_branch_history_length
      ; instruction_window_size
      ; jump_history_length
      ; jump_history_entry_width
      }
    { I.clock
    ; clear
    ; next_fetch_program_counter
    ; retirement_program_counter
    ; speculative_fetch_update
    ; speculative_decode_update
    ; retirement_update
    ; restore_from_decode
    ; restore_from_retirement
    }
    =
    let open Signal in
    let open Internal (struct
      let num_banks = num_banks
      let num_entries_per_bank = num_entries_per_bank
      let counter_width = counter_width
      let tag_width = tag_width
      let controlled_allocation_throttler_max = controlled_allocation_throttler_max
      let max_banks_skipped_on_allocation = max_banks_skipped_on_allocation
      let meta_width = meta_width
      let num_bimodal_direction_entries = num_bimodal_direction_entries
      let num_bimodal_hysteresis_entries = num_bimodal_hysteresis_entries
      let bimodal_hysteresis_width = bimodal_hysteresis_width
      let smallest_branch_history_length = smallest_branch_history_length
      let largest_branch_history_length = largest_branch_history_length
      let instruction_window_size = instruction_window_size
      let jump_history_length = jump_history_length
      let jump_history_entry_width = jump_history_entry_width
    end) in
    let { Update.valid = retirement_update_valid; _ } = retirement_update in
    let ({ Update.valid = write_enable; _ } as retirement_update) =
      { (retirement_update
         |> Update.Of_signal.reg
              ~enable:retirement_update_valid
              (Reg_spec.create ~clock ()))
        with
        valid = retirement_update_valid |> reg (Reg_spec.create ~clock ~clear ())
      }
    in
    let { Update_history.O.prediction_indices
        ; prediction_tags
        ; retirement_indices
        ; retirement_tags
        }
      =
      Update_history.hierarchical
        scope
        { clock
        ; clear
        ; next_fetch_program_counter
        ; retirement_program_counter
        ; speculative_fetch_update
        ; speculative_decode_update
        ; retirement_update
        ; restore_from_decode =
            restore_from_decode |> reg (Reg_spec.create ~clock ~clear ())
        ; restore_from_retirement =
            restore_from_retirement |> reg (Reg_spec.create ~clock ~clear ())
        }
    in
    let bimodal_entry = Bimodal_entry.Of_signal.wires () in
    let prediction_bimodal_entry, retirement_bimodal_entry =
      let { Bimodal_entry.direction; hysteresis } = bimodal_entry in
      let prediction_direction, retirement_direction =
        match
          forwarding_ram
            scope
            ~clock
            ~name:"mems$bimodal_direction"
            ~collision_mode:Read_before_write
            ~size:num_bimodal_direction_entries
            ~write_ports:
              [| { write_clock = clock
                 ; write_address =
                     hash_program_counter
                       ~bits:(address_bits_for num_bimodal_direction_entries)
                       retirement_program_counter
                     |> reg ~enable:retirement_update_valid (Reg_spec.create ~clock ())
                 ; write_enable
                 ; write_data = direction
                 }
              |]
            ~read_ports:
              [| { read_clock = clock
                 ; read_address =
                     hash_program_counter
                       ~bits:(address_bits_for num_bimodal_direction_entries)
                       next_fetch_program_counter
                 ; read_enable = vdd
                 }
               ; { read_clock = clock
                 ; read_address =
                     hash_program_counter
                       ~bits:(address_bits_for num_bimodal_direction_entries)
                       retirement_program_counter
                 ; read_enable = retirement_update_valid
                 }
              |]
            ()
        with
        | [| data1; data2 |] -> data1, data2
        | _ -> failwith "Code out of date"
      in
      let prediction_hysteresis, retirement_hysteresis =
        match
          forwarding_ram
            scope
            ~clock
            ~name:"mems$bimodal_hysteresis"
            ~collision_mode:Read_before_write
            ~size:num_bimodal_hysteresis_entries
            ~write_ports:
              [| { write_clock = clock
                 ; write_address =
                     hash_program_counter
                       ~bits:(address_bits_for num_bimodal_hysteresis_entries)
                       retirement_program_counter
                     |> reg ~enable:retirement_update_valid (Reg_spec.create ~clock ())
                 ; write_enable
                 ; write_data = hysteresis
                 }
              |]
            ~read_ports:
              [| { read_clock = clock
                 ; read_address =
                     hash_program_counter
                       ~bits:(address_bits_for num_bimodal_hysteresis_entries)
                       next_fetch_program_counter
                 ; read_enable = vdd
                 }
               ; { read_clock = clock
                 ; read_address =
                     hash_program_counter
                       ~bits:(address_bits_for num_bimodal_hysteresis_entries)
                       retirement_program_counter
                 ; read_enable = retirement_update_valid
                 }
              |]
            ()
        with
        | [| data1; data2 |] -> data1, data2
        | _ -> failwith "Code out of date"
      in
      ( { Bimodal_entry.direction = prediction_direction
        ; hysteresis = prediction_hysteresis
        }
      , { Bimodal_entry.direction = retirement_direction
        ; hysteresis = retirement_hysteresis
        } )
    in
    let write_entries = List.init num_banks ~f:(fun _ -> Entry.Of_signal.wires ()) in
    let prediction_read_entries, retirement_read_entries =
      List.zip_exn prediction_indices retirement_indices
      |> List.zip_exn write_entries
      |> List.mapi ~f:(fun bank (write_entry, (prediction_index, retirement_index)) ->
           match
             forwarding_ram
               scope
               ~clock
               ~name:(Printf.sprintf "mems$tage%d" bank)
               ~collision_mode:Read_before_write
               ~size:num_entries_per_bank
               ~write_ports:
                 [| { write_clock = clock
                    ; write_address =
                        retirement_index
                        |> reg ~enable:retirement_update_valid (Reg_spec.create ~clock ())
                    ; write_enable
                    ; write_data = Entry.Of_signal.pack write_entry
                    }
                 |]
               ~read_ports:
                 [| { read_clock = clock
                    ; read_address = prediction_index
                    ; read_enable = vdd
                    }
                  ; { read_clock = clock
                    ; read_address = retirement_index
                    ; read_enable = retirement_update_valid
                    }
                 |]
               ()
           with
           | [| data1; data2 |] ->
             Entry.Of_signal.unpack data1, Entry.Of_signal.unpack data2
           | _ -> failwith "Code out of date")
      |> List.unzip
    in
    let { Update_counters.O.next_entries; next_bimodal_entry; meta } =
      Update_counters.hierarchical
        scope
        { clock
        ; clear
        ; update = retirement_update
        ; read_entries = retirement_read_entries
        ; tags =
            retirement_tags
            |> List.map
                 ~f:(reg ~enable:retirement_update_valid (Reg_spec.create ~clock ()))
        ; bimodal_entry = retirement_bimodal_entry
        }
    in
    Bimodal_entry.Of_signal.assign bimodal_entry next_bimodal_entry;
    List.iter2_exn write_entries next_entries ~f:Entry.Of_signal.assign;
    let { Predict.O.taken = predicted_direction } =
      Predict.hierarchical
        scope
        { read_entries = prediction_read_entries
        ; tags = prediction_tags |> List.map ~f:(reg (Reg_spec.create ~clock ()))
        ; bimodal_entry = prediction_bimodal_entry
        ; meta
        }
    in
    { O.predicted_direction }
  ;;

  let hierarchical ~name ~params scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name (create ~params)
  ;;

  module Tests = struct
    module Params = struct
      let num_banks = 3
      let num_entries_per_bank = 128
      let counter_width = 2
      let tag_width = 7
      let controlled_allocation_throttler_max = 128
      let max_banks_skipped_on_allocation = 1
      let meta_width = 4
      let num_bimodal_direction_entries = 128
      let num_bimodal_hysteresis_entries = 64
      let bimodal_hysteresis_width = 1
      let smallest_branch_history_length = 2
      let largest_branch_history_length = 17
      let instruction_window_size = 0
      let jump_history_length = 3
      let jump_history_entry_width = 6
    end

    open Internal (Params)

    module Dual_counter : sig end = struct
      let test_bench ~f (sim : (Cyclesim.Port_list.t, Cyclesim.Port_list.t) Cyclesim.t) =
        let inputs, outputs = Cyclesim.inputs sim, Cyclesim.outputs sim in
        let print_state () =
          Stdio.print_s
            [%message
              (inputs : Cyclesim.Port_list.t)
                (outputs : Cyclesim.Port_list.t)
                ~internals:(Cyclesim.internal_ports sim : (string * Bits.t ref) list)];
          Stdio.print_endline "";
          ()
        in
        Cyclesim.reset sim;
        f print_state sim;
        ()
      ;;

      let sim ~circuit ~f =
        let sim =
          Cyclesim.create
            ~config:Cyclesim.Config.trace_all
            (Circuit.create_exn ~name:"test" circuit)
        in
        test_bench ~f sim;
        ()
      ;;

      let inputs =
        let open Signal in
        let circuit_inputs =
          List.map2_exn
            Dual_counter.(port_names |> to_list)
            Dual_counter.(port_widths |> to_list)
            ~f:(fun n b -> n, input n b)
        in
        Dual_counter.(of_alist circuit_inputs |> map ~f:wireof)
      ;;

      let test_all_inputs print_state sim =
        let open Bits in
        let num_takens = Cyclesim.in_port sim "num_takens" in
        let num_not_takens = Cyclesim.in_port sim "num_not_takens" in
        List.init (Int.shift_left 1 Params.counter_width) ~f:Fn.id
        |> List.iter ~f:(fun a ->
             List.init (Int.shift_left 1 Params.counter_width) ~f:Fn.id
             |> List.iter ~f:(fun b ->
                  num_takens := of_int ~width:Params.counter_width b;
                  num_not_takens := of_int ~width:Params.counter_width a;
                  Cyclesim.cycle sim;
                  print_state ();
                  ()));
        ()
      ;;

      let sim_io ~f =
        sim
          ~circuit:
            (List.map2_exn
               Dual_counter.(port_names |> to_list)
               Dual_counter.(f inputs |> to_list)
               ~f:(fun n s -> Signal.output ("out_" ^ n) s))
          ~f:test_all_inputs;
        ()
      ;;

      let sim_out ~f =
        sim ~circuit:[ f inputs |> Signal.output "out" ] ~f:test_all_inputs;
        ()
      ;;

      let%expect_test "Update taken" =
        sim_io ~f:(Dual_counter.update ~resolved_direction:Signal.vdd);
        [%expect
          {|
          ((inputs ((num_not_takens 00) (num_takens 00)))
           (outputs ((out_num_takens 01) (out_num_not_takens 00))) (internals ()))

          ((inputs ((num_not_takens 00) (num_takens 01)))
           (outputs ((out_num_takens 10) (out_num_not_takens 00))) (internals ()))

          ((inputs ((num_not_takens 00) (num_takens 10)))
           (outputs ((out_num_takens 11) (out_num_not_takens 00))) (internals ()))

          ((inputs ((num_not_takens 00) (num_takens 11)))
           (outputs ((out_num_takens 11) (out_num_not_takens 00))) (internals ()))

          ((inputs ((num_not_takens 01) (num_takens 00)))
           (outputs ((out_num_takens 01) (out_num_not_takens 01))) (internals ()))

          ((inputs ((num_not_takens 01) (num_takens 01)))
           (outputs ((out_num_takens 10) (out_num_not_takens 01))) (internals ()))

          ((inputs ((num_not_takens 01) (num_takens 10)))
           (outputs ((out_num_takens 11) (out_num_not_takens 01))) (internals ()))

          ((inputs ((num_not_takens 01) (num_takens 11)))
           (outputs ((out_num_takens 11) (out_num_not_takens 00))) (internals ()))

          ((inputs ((num_not_takens 10) (num_takens 00)))
           (outputs ((out_num_takens 01) (out_num_not_takens 10))) (internals ()))

          ((inputs ((num_not_takens 10) (num_takens 01)))
           (outputs ((out_num_takens 10) (out_num_not_takens 10))) (internals ()))

          ((inputs ((num_not_takens 10) (num_takens 10)))
           (outputs ((out_num_takens 11) (out_num_not_takens 10))) (internals ()))

          ((inputs ((num_not_takens 10) (num_takens 11)))
           (outputs ((out_num_takens 11) (out_num_not_takens 01))) (internals ()))

          ((inputs ((num_not_takens 11) (num_takens 00)))
           (outputs ((out_num_takens 01) (out_num_not_takens 11))) (internals ()))

          ((inputs ((num_not_takens 11) (num_takens 01)))
           (outputs ((out_num_takens 10) (out_num_not_takens 11))) (internals ()))

          ((inputs ((num_not_takens 11) (num_takens 10)))
           (outputs ((out_num_takens 11) (out_num_not_takens 11))) (internals ()))

          ((inputs ((num_not_takens 11) (num_takens 11)))
           (outputs ((out_num_takens 11) (out_num_not_takens 10))) (internals ())) |}]
      ;;

      let%expect_test "Update not taken" =
        sim_io ~f:(Dual_counter.update ~resolved_direction:Signal.gnd);
        [%expect
          {|
          ((inputs ((num_takens 00) (num_not_takens 00)))
           (outputs ((out_num_takens 00) (out_num_not_takens 01))) (internals ()))

          ((inputs ((num_takens 01) (num_not_takens 00)))
           (outputs ((out_num_takens 01) (out_num_not_takens 01))) (internals ()))

          ((inputs ((num_takens 10) (num_not_takens 00)))
           (outputs ((out_num_takens 10) (out_num_not_takens 01))) (internals ()))

          ((inputs ((num_takens 11) (num_not_takens 00)))
           (outputs ((out_num_takens 11) (out_num_not_takens 01))) (internals ()))

          ((inputs ((num_takens 00) (num_not_takens 01)))
           (outputs ((out_num_takens 00) (out_num_not_takens 10))) (internals ()))

          ((inputs ((num_takens 01) (num_not_takens 01)))
           (outputs ((out_num_takens 01) (out_num_not_takens 10))) (internals ()))

          ((inputs ((num_takens 10) (num_not_takens 01)))
           (outputs ((out_num_takens 10) (out_num_not_takens 10))) (internals ()))

          ((inputs ((num_takens 11) (num_not_takens 01)))
           (outputs ((out_num_takens 11) (out_num_not_takens 10))) (internals ()))

          ((inputs ((num_takens 00) (num_not_takens 10)))
           (outputs ((out_num_takens 00) (out_num_not_takens 11))) (internals ()))

          ((inputs ((num_takens 01) (num_not_takens 10)))
           (outputs ((out_num_takens 01) (out_num_not_takens 11))) (internals ()))

          ((inputs ((num_takens 10) (num_not_takens 10)))
           (outputs ((out_num_takens 10) (out_num_not_takens 11))) (internals ()))

          ((inputs ((num_takens 11) (num_not_takens 10)))
           (outputs ((out_num_takens 11) (out_num_not_takens 11))) (internals ()))

          ((inputs ((num_takens 00) (num_not_takens 11)))
           (outputs ((out_num_takens 00) (out_num_not_takens 11))) (internals ()))

          ((inputs ((num_takens 01) (num_not_takens 11)))
           (outputs ((out_num_takens 00) (out_num_not_takens 11))) (internals ()))

          ((inputs ((num_takens 10) (num_not_takens 11)))
           (outputs ((out_num_takens 01) (out_num_not_takens 11))) (internals ()))

          ((inputs ((num_takens 11) (num_not_takens 11)))
           (outputs ((out_num_takens 10) (out_num_not_takens 11))) (internals ())) |}]
      ;;

      let%expect_test "Decay" =
        sim_io ~f:Dual_counter.decay;
        [%expect
          {|
          ((inputs ((num_takens 00) (num_not_takens 00)))
           (outputs ((out_num_takens 00) (out_num_not_takens 00))) (internals ()))

          ((inputs ((num_takens 01) (num_not_takens 00)))
           (outputs ((out_num_takens 00) (out_num_not_takens 00))) (internals ()))

          ((inputs ((num_takens 10) (num_not_takens 00)))
           (outputs ((out_num_takens 01) (out_num_not_takens 00))) (internals ()))

          ((inputs ((num_takens 11) (num_not_takens 00)))
           (outputs ((out_num_takens 10) (out_num_not_takens 00))) (internals ()))

          ((inputs ((num_takens 00) (num_not_takens 01)))
           (outputs ((out_num_takens 00) (out_num_not_takens 00))) (internals ()))

          ((inputs ((num_takens 01) (num_not_takens 01)))
           (outputs ((out_num_takens 01) (out_num_not_takens 01))) (internals ()))

          ((inputs ((num_takens 10) (num_not_takens 01)))
           (outputs ((out_num_takens 01) (out_num_not_takens 01))) (internals ()))

          ((inputs ((num_takens 11) (num_not_takens 01)))
           (outputs ((out_num_takens 10) (out_num_not_takens 01))) (internals ()))

          ((inputs ((num_takens 00) (num_not_takens 10)))
           (outputs ((out_num_takens 00) (out_num_not_takens 01))) (internals ()))

          ((inputs ((num_takens 01) (num_not_takens 10)))
           (outputs ((out_num_takens 01) (out_num_not_takens 01))) (internals ()))

          ((inputs ((num_takens 10) (num_not_takens 10)))
           (outputs ((out_num_takens 10) (out_num_not_takens 10))) (internals ()))

          ((inputs ((num_takens 11) (num_not_takens 10)))
           (outputs ((out_num_takens 10) (out_num_not_takens 10))) (internals ()))

          ((inputs ((num_takens 00) (num_not_takens 11)))
           (outputs ((out_num_takens 00) (out_num_not_takens 10))) (internals ()))

          ((inputs ((num_takens 01) (num_not_takens 11)))
           (outputs ((out_num_takens 01) (out_num_not_takens 10))) (internals ()))

          ((inputs ((num_takens 10) (num_not_takens 11)))
           (outputs ((out_num_takens 10) (out_num_not_takens 10))) (internals ()))

          ((inputs ((num_takens 11) (num_not_takens 11)))
           (outputs ((out_num_takens 11) (out_num_not_takens 11))) (internals ())) |}]
      ;;

      let%expect_test "Prediction" =
        sim_out ~f:(Dual_counter.confidence_level ~meta:(Signal.zero 1));
        [%expect
          {|
          ((inputs ((num_not_takens 00) (num_takens 00))) (outputs ((out 11)))
           (internals ((gnd 0))))

          ((inputs ((num_not_takens 00) (num_takens 01))) (outputs ((out 00)))
           (internals ((gnd 0))))

          ((inputs ((num_not_takens 00) (num_takens 10))) (outputs ((out 00)))
           (internals ((gnd 0))))

          ((inputs ((num_not_takens 00) (num_takens 11))) (outputs ((out 00)))
           (internals ((gnd 0))))

          ((inputs ((num_not_takens 01) (num_takens 00))) (outputs ((out 00)))
           (internals ((gnd 0))))

          ((inputs ((num_not_takens 01) (num_takens 01))) (outputs ((out 11)))
           (internals ((gnd 0))))

          ((inputs ((num_not_takens 01) (num_takens 10))) (outputs ((out 10)))
           (internals ((gnd 0))))

          ((inputs ((num_not_takens 01) (num_takens 11))) (outputs ((out 01)))
           (internals ((gnd 0))))

          ((inputs ((num_not_takens 10) (num_takens 00))) (outputs ((out 00)))
           (internals ((gnd 0))))

          ((inputs ((num_not_takens 10) (num_takens 01))) (outputs ((out 10)))
           (internals ((gnd 0))))

          ((inputs ((num_not_takens 10) (num_takens 10))) (outputs ((out 11)))
           (internals ((gnd 0))))

          ((inputs ((num_not_takens 10) (num_takens 11))) (outputs ((out 10)))
           (internals ((gnd 0))))

          ((inputs ((num_not_takens 11) (num_takens 00))) (outputs ((out 00)))
           (internals ((gnd 0))))

          ((inputs ((num_not_takens 11) (num_takens 01))) (outputs ((out 01)))
           (internals ((gnd 0))))

          ((inputs ((num_not_takens 11) (num_takens 10))) (outputs ((out 10)))
           (internals ((gnd 0))))

          ((inputs ((num_not_takens 11) (num_takens 11))) (outputs ((out 11)))
           (internals ((gnd 0)))) |}]
      ;;
    end

    module Update_history : sig end = struct
      open Update_history

      let test_bench ~debug ~f (sim : (_ I.t, _ O.t) Cyclesim.t) =
        let inputs, outputs = Cyclesim.inputs sim, Cyclesim.outputs sim in
        let print_state () =
          Stdio.print_s
            [%message
              (inputs : Bits.t ref I.t)
                (outputs : Bits.t ref O.t)
                ~internals:
                  (if debug
                   then
                     Cyclesim.internal_ports sim
                     |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
                   else []
                    : (string * Bits.t ref) list)];
          Stdio.print_endline "";
          ()
        in
        Cyclesim.reset sim;
        f print_state sim;
        ()
      ;;

      let sim ~debug ~f =
        let module Simulator = Cyclesim.With_interface (I) (O) in
        let scope = Scope.create ~flatten_design:true () in
        let sim =
          Simulator.create
            ~config:Cyclesim.Config.trace_all
            (create ~hash:(fun ~bits address -> Signal.sel_bottom address bits) scope)
        in
        test_bench ~debug ~f sim;
        ()
      ;;

      module Model = struct
        type t =
          { branch_history_pointer : int ref
          ; jump_history_pointer : int ref
          ; branch_history : int array
          ; jump_history : int array
          ; branch_index_fold : int array
          ; jump_index_fold : int array
          ; branch_tag_fold : int array
          ; jump_tag_fold : int array
          }
        [@@deriving sexp_of]

        let init () =
          { branch_history_pointer = ref 0
          ; jump_history_pointer = ref 0
          ; branch_history = Array.create ~len:Params.largest_branch_history_length 0
          ; jump_history = Array.create ~len:Params.jump_history_length 0
          ; branch_index_fold = Array.create ~len:Params.num_banks 0
          ; jump_index_fold = Array.create ~len:Params.num_banks 0
          ; branch_tag_fold = Array.create ~len:Params.num_banks 0
          ; jump_tag_fold = Array.create ~len:Params.num_banks 0
          }
        ;;

        let update
          (retirement_program_counter, valid, resolved_direction, branch_target)
          { branch_history_pointer
          ; jump_history_pointer
          ; branch_history
          ; jump_history
          ; branch_index_fold
          ; jump_index_fold
          ; branch_tag_fold
          ; jump_tag_fold
          }
          =
          if valid
          then (
            let _insert =
              Array.set
                branch_history
                !branch_history_pointer
                (if resolved_direction then 1 else 0);
              Array.set jump_history !jump_history_pointer branch_target;
              ()
            in
            let _update_folds =
              for i = 0 to Params.num_banks - 1 do
                let update
                  ~history
                  ~ptr
                  ~fold
                  ~original_length
                  ~compressed_length
                  ~injected_bits
                  ~outpoint
                  =
                  let rotate_left x m =
                    let y = Int.shift_right_logical x (compressed_length - m) in
                    let x = Int.shift_left x m |> Int.bit_or y in
                    Int.bit_and x (Int.shift_left 1 compressed_length - 1)
                  in
                  let get offset =
                    let k = ptr + offset in
                    let k =
                      if k >= Array.length history then k - Array.length history else k
                    in
                    Array.get history k
                  in
                  let new_fold = rotate_left (Array.get fold i) 1 in
                  let inbits =
                    get 0 |> Int.bit_and (Int.shift_left 1 injected_bits - 1)
                  in
                  let outbits =
                    get (original_length - 1)
                    |> Int.bit_and (Int.shift_left 1 injected_bits - 1)
                  in
                  let outbits = rotate_left outbits outpoint in
                  Array.set fold i (Int.bit_xor new_fold inbits |> Int.bit_xor outbits);
                  ()
                in
                update
                  ~history:branch_history
                  ~ptr:!branch_history_pointer
                  ~fold:branch_index_fold
                  ~original_length:
                    (List.nth_exn folded_histories i).branch.index.original_length
                  ~compressed_length:
                    (List.nth_exn folded_histories i).branch.index.compressed_length
                  ~injected_bits:
                    (List.nth_exn folded_histories i).branch.index.injected_bits
                  ~outpoint:(List.nth_exn folded_histories i).branch.index.out_point;
                update
                  ~history:branch_history
                  ~ptr:!branch_history_pointer
                  ~fold:branch_tag_fold
                  ~original_length:
                    (List.nth_exn folded_histories i).branch.tag.original_length
                  ~compressed_length:
                    (List.nth_exn folded_histories i).branch.tag.compressed_length
                  ~injected_bits:
                    (List.nth_exn folded_histories i).branch.tag.injected_bits
                  ~outpoint:(List.nth_exn folded_histories i).branch.tag.out_point;
                update
                  ~history:jump_history
                  ~ptr:!jump_history_pointer
                  ~fold:jump_index_fold
                  ~original_length:
                    (List.nth_exn folded_histories i).jump.index.original_length
                  ~compressed_length:
                    (List.nth_exn folded_histories i).jump.index.compressed_length
                  ~injected_bits:
                    (List.nth_exn folded_histories i).jump.index.injected_bits
                  ~outpoint:(List.nth_exn folded_histories i).jump.index.out_point;
                update
                  ~history:jump_history
                  ~ptr:!jump_history_pointer
                  ~fold:jump_tag_fold
                  ~original_length:
                    (List.nth_exn folded_histories i).jump.tag.original_length
                  ~compressed_length:
                    (List.nth_exn folded_histories i).jump.tag.compressed_length
                  ~injected_bits:(List.nth_exn folded_histories i).jump.tag.injected_bits
                  ~outpoint:(List.nth_exn folded_histories i).jump.tag.out_point;
                ()
              done;
              ()
            in
            let _update_pointers =
              let update_pointer ~ptr ~max =
                ptr := !ptr - 1;
                if !ptr < 0 then ptr := max - 1;
                ()
              in
              update_pointer
                ~ptr:branch_history_pointer
                ~max:Params.largest_branch_history_length;
              update_pointer ~ptr:jump_history_pointer ~max:Params.jump_history_length;
              ()
            in
            ());
          ( List.init Params.num_banks ~f:(fun i ->
              Int.bit_xor retirement_program_counter i
              |> Int.bit_and (Int.shift_left 1 Params.bank_address_width - 1)
              |> Int.bit_xor branch_index_fold.(i)
              |> Int.bit_xor
                   (Int.shift_left
                      jump_index_fold.(i)
                      ((List.nth_exn folded_histories i).branch.index.compressed_length
                       - (List.nth_exn folded_histories i).jump.index.compressed_length)))
          , List.init Params.num_banks ~f:(fun i ->
              let reverse x nbits =
                (* reverse the 16 rightmost bits (Strachey's method) *)
                assert (nbits <= 16);
                let x = x land 0xFFFF in
                let x = x lor ((x land 0x000000FF) lsl 16) in
                let x = x land 0xF0F0F0F0 lor ((x land 0x0F0F0F0F) lsl 8) in
                let x = x land 0xCCCCCCCC lor ((x land 0x33333333) lsl 4) in
                let x = x land 0xAAAAAAAA lor ((x land 0x55555555) lsl 2) in
                x lsr (31 - nbits)
              in
              retirement_program_counter + i
              |> Int.bit_and (Int.shift_left 1 Params.tag_width - 1)
              |> Int.bit_xor
                   (reverse
                      branch_tag_fold.(i)
                      (List.nth_exn folded_histories i).branch.tag.compressed_length)
              |> Int.bit_xor
                   (Int.shift_left
                      jump_tag_fold.(i)
                      ((List.nth_exn folded_histories i).branch.tag.compressed_length
                       - (List.nth_exn folded_histories i).jump.tag.compressed_length))) )
        ;;
      end

      let%expect_test "Model" =
        let open Bits in
        let open Quickcheck in
        sim ~debug:true ~f:(fun _print_state sim ->
          let inputs = Cyclesim.inputs sim in
          let outputs = Cyclesim.outputs sim in
          Cyclesim.reset sim;
          inputs.clear := vdd;
          Cyclesim.cycle sim;
          inputs.clear := gnd;
          Cyclesim.cycle sim;
          let model = Model.init () in
          Quickcheck.test
            (List.quickcheck_generator
               (Generator.tuple4
                  Int.quickcheck_generator
                  Bool.quickcheck_generator
                  Bool.quickcheck_generator
                  Int.quickcheck_generator))
            ~sexp_of:[%sexp_of: (int * bool * bool * int) list]
            ~f:(fun updates ->
              List.iter
                updates
                ~f:(fun
                     (( retirement_program_counter
                      , valid
                      , resolved_direction
                      , branch_target ) as model_inputs)
                   ->
                inputs.retirement_program_counter
                  := of_int
                       ~width:(width !(inputs.retirement_program_counter))
                       retirement_program_counter;
                inputs.retirement_update.valid := of_bool valid;
                inputs.retirement_update.resolved_direction := of_bool resolved_direction;
                inputs.retirement_update.branch_target
                  := of_int
                       ~width:(width !(inputs.retirement_update.branch_target))
                       branch_target;
                Cyclesim.cycle sim;
                inputs.retirement_update.valid := gnd;
                Cyclesim.cycle sim;
                inputs.retirement_update.valid := of_bool valid;
                let expected_indices, expected_tags = Model.update model_inputs model in
                let { O.prediction_indices = _
                    ; prediction_tags = _
                    ; retirement_indices = indices
                    ; retirement_tags = tags
                    }
                  =
                  O.map outputs ~f:(( ! ) |> Fn.compose to_int)
                in
                [%test_result: int list * int list]
                  ~message:([%message "" (model : Model.t)] |> Sexp.to_string_hum)
                  ~expect:(expected_indices, expected_tags)
                  (indices, tags);
                ());
              ());
          ());
        ()
      ;;

      let%expect_test "Human" =
        Stdio.print_s
          [%message
            (folded_histories : Folded_history_metadata.t Indices_and_tags.t list)];
        Stdio.print_endline "";
        sim ~debug:false ~f:(fun print_state sim ->
          let open Bits in
          let inputs = Cyclesim.inputs sim in
          let next () =
            Cyclesim.cycle sim;
            print_state ();
            ()
          in
          next ();
          inputs.retirement_update.valid := vdd;
          inputs.retirement_update.resolved_direction := vdd;
          next ();
          next ();
          next ();
          inputs.retirement_update.valid := gnd;
          next ();
          next ();
          inputs.retirement_update.valid := vdd;
          inputs.retirement_update.resolved_direction := gnd;
          next ();
          next ();
          inputs.retirement_update.valid := gnd;
          next ();
          next ();
          inputs.restore_from_retirement := vdd;
          next ();
          ());
        [%expect
          {|
          (folded_histories
           (((branch
              ((index
                ((original_length 17) (compressed_length 7) (out_point 3)
                 (injected_bits 1)))
               (tag
                ((original_length 17) (compressed_length 7) (out_point 3)
                 (injected_bits 1)))))
             (jump
              ((index
                ((original_length 3) (compressed_length 5) (out_point 3)
                 (injected_bits 1)))
               (tag
                ((original_length 3) (compressed_length 6) (out_point 3)
                 (injected_bits 1))))))
            ((branch
              ((index
                ((original_length 5) (compressed_length 7) (out_point 5)
                 (injected_bits 1)))
               (tag
                ((original_length 5) (compressed_length 7) (out_point 5)
                 (injected_bits 1)))))
             (jump
              ((index
                ((original_length 3) (compressed_length 5) (out_point 3)
                 (injected_bits 1)))
               (tag
                ((original_length 3) (compressed_length 6) (out_point 3)
                 (injected_bits 1))))))
            ((branch
              ((index
                ((original_length 2) (compressed_length 7) (out_point 2)
                 (injected_bits 1)))
               (tag
                ((original_length 2) (compressed_length 7) (out_point 2)
                 (injected_bits 1)))))
             (jump
              ((index
                ((original_length 2) (compressed_length 5) (out_point 2)
                 (injected_bits 5)))
               (tag
                ((original_length 2) (compressed_length 6) (out_point 2)
                 (injected_bits 6))))))))

          ((inputs
            ((clock 0) (clear 0)
             (next_fetch_program_counter 00000000000000000000000000000000)
             (retirement_program_counter 00000000000000000000000000000000)
             (speculative_fetch_update
              ((valid 0) (resolved_direction 0)
               (branch_target 00000000000000000000000000000000)))
             (speculative_decode_update
              ((valid 0) (resolved_direction 0)
               (branch_target 00000000000000000000000000000000)))
             (retirement_update
              ((valid 0) (resolved_direction 0)
               (branch_target 00000000000000000000000000000000)))
             (restore_from_decode 0) (restore_from_retirement 0)))
           (outputs
            ((prediction_indices (0000000 0000001 0000010))
             (prediction_tags (0000000 0000001 0000010))
             (retirement_indices (0000000 0000001 0000010))
             (retirement_tags (0000000 0000001 0000010))))
           (internals ()))

          ((inputs
            ((clock 0) (clear 0)
             (next_fetch_program_counter 00000000000000000000000000000000)
             (retirement_program_counter 00000000000000000000000000000000)
             (speculative_fetch_update
              ((valid 0) (resolved_direction 0)
               (branch_target 00000000000000000000000000000000)))
             (speculative_decode_update
              ((valid 0) (resolved_direction 0)
               (branch_target 00000000000000000000000000000000)))
             (retirement_update
              ((valid 1) (resolved_direction 1)
               (branch_target 00000000000000000000000000000000)))
             (restore_from_decode 0) (restore_from_retirement 0)))
           (outputs
            ((prediction_indices (0000000 0000001 0000010))
             (prediction_tags (0000000 0000001 0000010))
             (retirement_indices (0000011 0000010 0000101))
             (retirement_tags (1100000 1100001 1110010))))
           (internals ()))

          ((inputs
            ((clock 0) (clear 0)
             (next_fetch_program_counter 00000000000000000000000000000000)
             (retirement_program_counter 00000000000000000000000000000000)
             (speculative_fetch_update
              ((valid 0) (resolved_direction 0)
               (branch_target 00000000000000000000000000000000)))
             (speculative_decode_update
              ((valid 0) (resolved_direction 0)
               (branch_target 00000000000000000000000000000000)))
             (retirement_update
              ((valid 1) (resolved_direction 1)
               (branch_target 00000000000000000000000000000000)))
             (restore_from_decode 0) (restore_from_retirement 0)))
           (outputs
            ((prediction_indices (0000000 0000001 0000010))
             (prediction_tags (0000000 0000001 0000010))
             (retirement_indices (0000111 0000110 0001001))
             (retirement_tags (1110000 1110001 1101010))))
           (internals ()))

          ((inputs
            ((clock 0) (clear 0)
             (next_fetch_program_counter 00000000000000000000000000000000)
             (retirement_program_counter 00000000000000000000000000000000)
             (speculative_fetch_update
              ((valid 0) (resolved_direction 0)
               (branch_target 00000000000000000000000000000000)))
             (speculative_decode_update
              ((valid 0) (resolved_direction 0)
               (branch_target 00000000000000000000000000000000)))
             (retirement_update
              ((valid 1) (resolved_direction 1)
               (branch_target 00000000000000000000000000000000)))
             (restore_from_decode 0) (restore_from_retirement 0)))
           (outputs
            ((prediction_indices (0000000 0000001 0000010))
             (prediction_tags (0000000 0000001 0000010))
             (retirement_indices (0001111 0001110 0010001))
             (retirement_tags (1111000 1111001 1100110))))
           (internals ()))

          ((inputs
            ((clock 0) (clear 0)
             (next_fetch_program_counter 00000000000000000000000000000000)
             (retirement_program_counter 00000000000000000000000000000000)
             (speculative_fetch_update
              ((valid 0) (resolved_direction 0)
               (branch_target 00000000000000000000000000000000)))
             (speculative_decode_update
              ((valid 0) (resolved_direction 0)
               (branch_target 00000000000000000000000000000000)))
             (retirement_update
              ((valid 0) (resolved_direction 1)
               (branch_target 00000000000000000000000000000000)))
             (restore_from_decode 0) (restore_from_retirement 0)))
           (outputs
            ((prediction_indices (0000000 0000001 0000010))
             (prediction_tags (0000000 0000001 0000010))
             (retirement_indices (0000111 0000110 0001001))
             (retirement_tags (1110000 1110001 1101010))))
           (internals ()))

          ((inputs
            ((clock 0) (clear 0)
             (next_fetch_program_counter 00000000000000000000000000000000)
             (retirement_program_counter 00000000000000000000000000000000)
             (speculative_fetch_update
              ((valid 0) (resolved_direction 0)
               (branch_target 00000000000000000000000000000000)))
             (speculative_decode_update
              ((valid 0) (resolved_direction 0)
               (branch_target 00000000000000000000000000000000)))
             (retirement_update
              ((valid 0) (resolved_direction 1)
               (branch_target 00000000000000000000000000000000)))
             (restore_from_decode 0) (restore_from_retirement 0)))
           (outputs
            ((prediction_indices (0000000 0000001 0000010))
             (prediction_tags (0000000 0000001 0000010))
             (retirement_indices (0000111 0000110 0001001))
             (retirement_tags (1110000 1110001 1101010))))
           (internals ()))

          ((inputs
            ((clock 0) (clear 0)
             (next_fetch_program_counter 00000000000000000000000000000000)
             (retirement_program_counter 00000000000000000000000000000000)
             (speculative_fetch_update
              ((valid 0) (resolved_direction 0)
               (branch_target 00000000000000000000000000000000)))
             (speculative_decode_update
              ((valid 0) (resolved_direction 0)
               (branch_target 00000000000000000000000000000000)))
             (retirement_update
              ((valid 1) (resolved_direction 0)
               (branch_target 00000000000000000000000000000000)))
             (restore_from_decode 0) (restore_from_retirement 0)))
           (outputs
            ((prediction_indices (0000000 0000001 0000010))
             (prediction_tags (0000000 0000001 0000010))
             (retirement_indices (0011100 0111101 0100110))
             (retirement_tags (0011100 0011111 0010000))))
           (internals ()))

          ((inputs
            ((clock 0) (clear 0)
             (next_fetch_program_counter 00000000000000000000000000000000)
             (retirement_program_counter 00000000000000000000000000000000)
             (speculative_fetch_update
              ((valid 0) (resolved_direction 0)
               (branch_target 00000000000000000000000000000000)))
             (speculative_decode_update
              ((valid 0) (resolved_direction 0)
               (branch_target 00000000000000000000000000000000)))
             (retirement_update
              ((valid 1) (resolved_direction 0)
               (branch_target 00000000000000000000000000000000)))
             (restore_from_decode 0) (restore_from_retirement 0)))
           (outputs
            ((prediction_indices (0000000 0000001 0000010))
             (prediction_tags (0000000 0000001 0000010))
             (retirement_indices (0111000 1011001 1001010))
             (retirement_tags (0001110 0001100 0001011))))
           (internals ()))

          ((inputs
            ((clock 0) (clear 0)
             (next_fetch_program_counter 00000000000000000000000000000000)
             (retirement_program_counter 00000000000000000000000000000000)
             (speculative_fetch_update
              ((valid 0) (resolved_direction 0)
               (branch_target 00000000000000000000000000000000)))
             (speculative_decode_update
              ((valid 0) (resolved_direction 0)
               (branch_target 00000000000000000000000000000000)))
             (retirement_update
              ((valid 0) (resolved_direction 0)
               (branch_target 00000000000000000000000000000000)))
             (restore_from_decode 0) (restore_from_retirement 0)))
           (outputs
            ((prediction_indices (0000000 0000001 0000010))
             (prediction_tags (0000000 0000001 0000010))
             (retirement_indices (0011100 0111101 0100110))
             (retirement_tags (0011100 0011111 0010000))))
           (internals ()))

          ((inputs
            ((clock 0) (clear 0)
             (next_fetch_program_counter 00000000000000000000000000000000)
             (retirement_program_counter 00000000000000000000000000000000)
             (speculative_fetch_update
              ((valid 0) (resolved_direction 0)
               (branch_target 00000000000000000000000000000000)))
             (speculative_decode_update
              ((valid 0) (resolved_direction 0)
               (branch_target 00000000000000000000000000000000)))
             (retirement_update
              ((valid 0) (resolved_direction 0)
               (branch_target 00000000000000000000000000000000)))
             (restore_from_decode 0) (restore_from_retirement 0)))
           (outputs
            ((prediction_indices (0000000 0000001 0000010))
             (prediction_tags (0000000 0000001 0000010))
             (retirement_indices (0011100 0111101 0100110))
             (retirement_tags (0011100 0011111 0010000))))
           (internals ()))

          ((inputs
            ((clock 0) (clear 0)
             (next_fetch_program_counter 00000000000000000000000000000000)
             (retirement_program_counter 00000000000000000000000000000000)
             (speculative_fetch_update
              ((valid 0) (resolved_direction 0)
               (branch_target 00000000000000000000000000000000)))
             (speculative_decode_update
              ((valid 0) (resolved_direction 0)
               (branch_target 00000000000000000000000000000000)))
             (retirement_update
              ((valid 0) (resolved_direction 0)
               (branch_target 00000000000000000000000000000000)))
             (restore_from_decode 0) (restore_from_retirement 1)))
           (outputs
            ((prediction_indices (0011100 0111101 0100110))
             (prediction_tags (0011100 0011111 0010000))
             (retirement_indices (0011100 0111101 0100110))
             (retirement_tags (0011100 0011111 0010000))))
           (internals ())) |}]
      ;;
    end

    module Update_counters = struct
      open Update_counters

      module Dual_counter = struct
        module Model = struct
          type t =
            { num_takens : int
            ; num_not_takens : int
            }
          [@@deriving sexp_of, compare]

          let of_bimodal direction hysteresis =
            let n = Array.create ~len:2 0 in
            Array.set
              n
              (direction lxor 1)
              ((1 lsl (Params.bimodal_hysteresis_width - 1)) - 1);
            Array.set
              n
              direction
              (n.(direction lxor 1) + (1 lsl Params.bimodal_hysteresis_width) - hysteresis);
            { num_takens = n.(1); num_not_takens = n.(0) }
          ;;

          let prediction { num_takens; num_not_takens } =
            if num_takens > num_not_takens then 1 else 0
          ;;

          let is_medium_confidence { num_takens; num_not_takens } =
            num_takens = (2 * num_not_takens) + 1 || num_not_takens = (2 * num_takens) + 1
          ;;

          let is_low_confidence { num_takens; num_not_takens } =
            num_takens < (2 * num_not_takens) + 1 && num_not_takens < (2 * num_takens) + 1
          ;;

          let is_high_confidence counters =
            (not (is_medium_confidence counters)) && not (is_low_confidence counters)
          ;;

          let is_very_high_confidence { num_takens; num_not_takens } =
            (num_not_takens = 0 && num_takens >= 4)
            || (num_takens = 0 && num_not_takens >= 4)
          ;;

          let sum { num_takens; num_not_takens } = num_takens + num_not_takens
          let diff { num_takens; num_not_takens } = abs (num_takens - num_not_takens)

          let is_saturated { num_takens; num_not_takens } =
            num_takens = Params.max_counter_value
            || num_not_takens = Params.max_counter_value
          ;;

          let confidence_level meta ({ num_takens; num_not_takens } as counters) =
            if num_takens = num_not_takens
            then 3
            else (
              let low = is_low_confidence counters in
              let promotemed = meta >= 0 && sum counters = 1 in
              let med = (not promotemed) && is_medium_confidence counters in
              (if low then 2 else 0) + if med then 1 else 0)
          ;;

          let decay { num_takens; num_not_takens } =
            let n = List.to_array [ num_not_takens; num_takens ] in
            if n.(1) > n.(0) then Array.set n 1 (n.(1) - 1);
            if n.(0) > n.(1) then Array.set n 0 (n.(0) - 1);
            { num_takens = n.(1); num_not_takens = n.(0) }
          ;;

          let update taken { num_takens; num_not_takens } =
            let n = List.to_array [ num_not_takens; num_takens ] in
            let d = if taken then 1 else 0 in
            if n.(d) < Params.max_counter_value
            then Array.set n d (n.(d) + 1)
            else if n.(d lxor 1) > 0
            then Array.set n (d lxor 1) (n.(d lxor 1) - 1);
            { num_takens = n.(1); num_not_takens = n.(0) }
          ;;
        end
      end

      let test_bench ~debug ~f (sim : (_ I.t, _ O.t) Cyclesim.t) =
        let inputs, outputs = Cyclesim.inputs sim, Cyclesim.outputs sim in
        let print_state () =
          Stdio.print_s
            [%message
              (inputs : Bits.t ref I.t)
                (outputs : Bits.t ref O.t)
                ~internals:
                  (if debug
                   then
                     Cyclesim.internal_ports sim
                     |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
                   else []
                    : (string * Bits.t ref) list)];
          Stdio.print_endline "";
          ()
        in
        Cyclesim.reset sim;
        f print_state sim;
        ()
      ;;

      let sim ~debug ~f =
        let module Simulator = Cyclesim.With_interface (I) (O) in
        let scope = Scope.create ~flatten_design:true () in
        let sim = Simulator.create ~config:Cyclesim.Config.trace_all (create scope) in
        test_bench ~debug ~f sim;
        ()
      ;;

      module Model = struct
        type t =
          { meta : int ref
          ; cat : int ref
          ; random1 : int ref
          ; random2 : int ref
          ; random3 : int ref
          ; random4 : int ref
          ; random5 : int ref
          }
        [@@deriving sexp_of]

        let init () =
          { meta = ref 0
          ; cat = ref 0
          ; random1 = ref 2463534242
          ; random2 = ref 1850600128
          ; random3 = ref 3837179466
          ; random4 = ref 4290344314
          ; random5 = ref 0614373416
          }
        ;;

        let update
          resolved_direction
          entries
          (bimodal_direction, bimodal_hysteresis)
          { meta; cat; random1; random2; random3; random4; random5 }
          =
          let open Dual_counter.Model in
          let hits, s, bp =
            let hits =
              List.filter_mapi entries ~f:(fun i (_, hit) -> if hit then Some i else None)
            in
            let s =
              List.filter_map entries ~f:(fun (counters, hit) ->
                if hit then Some counters else None)
              |> Fn.flip
                   List.append
                   [ of_bimodal (if bimodal_direction then 1 else 0) bimodal_hysteresis ]
            in
            let bp = ref 0 in
            for i = 1 to List.length s - 1 do
              if confidence_level !meta (List.nth_exn s i)
                 < confidence_level !meta (List.nth_exn s !bp)
              then bp := i
            done;
            hits, s, !bp
          in
          let result_entries = List.map entries ~f:fst |> List.to_array in
          let result_bimodal_direction = ref bimodal_direction in
          let result_bimodal_hysteresis = ref bimodal_hysteresis in
          let _update_meta =
            if List.length s > 1
               && sum (List.hd_exn s) = 1
               && is_high_confidence (List.nth_exn s 1)
               && prediction (List.hd_exn s) <> prediction (List.nth_exn s 1)
            then
              if prediction (List.hd_exn s) = if resolved_direction then 1 else 0
              then
                if !meta < 15
                then meta := !meta + 1
                else if !meta > -16
                then meta := !meta - 1;
            ()
          in
          let _update =
            for i = 0 to bp - 1 do
              if !meta >= 0
                 || is_low_confidence (List.nth_exn s i)
                 || prediction (List.nth_exn s i) <> prediction (List.nth_exn s bp)
                 || !random1 % 8 = 0
              then
                Array.set
                  result_entries
                  (List.nth_exn hits i)
                  (update resolved_direction result_entries.(List.nth_exn hits i))
            done;
            let update_entry i =
              Stdio.print_s [%message "update_entry" (i : int)];
              let update_bimodal () =
                if Bool.equal bimodal_direction resolved_direction
                then (
                  if bimodal_hysteresis < (1 lsl Params.bimodal_hysteresis_width) - 1
                  then result_bimodal_hysteresis := bimodal_hysteresis + 1)
                else if bimodal_hysteresis > 0
                then result_bimodal_hysteresis := bimodal_hysteresis - 1
                else result_bimodal_direction := resolved_direction
              in
              if i < List.length hits
              then
                Array.set
                  result_entries
                  (List.nth_exn hits i)
                  (update resolved_direction result_entries.(List.nth_exn hits i))
              else update_bimodal ()
            in
            Stdio.print_s
              [%message
                (bp : int)
                  (List.nth hits bp : int option)
                  (List.length hits : int)
                  (entries : (t * bool) list)];
            if bp < List.length hits
               && is_high_confidence (List.nth_exn s bp)
               && is_high_confidence (List.nth_exn s (bp + 1))
               && (prediction (List.nth_exn s (bp + 1))
                   = if resolved_direction then 1 else 0)
               && ((prediction (List.nth_exn s bp) = if resolved_direction then 1 else 0)
                   || !cat >= Params.controlled_allocation_throttler_max / 2)
            then (
              Stdio.print_endline "inside";
              if (not (is_saturated (List.nth_exn s bp)))
                 || (!meta < 0 && !random2 % 8 = 0)
              then
                Array.set
                  result_entries
                  (List.nth_exn hits bp)
                  (decay result_entries.(List.nth_exn hits bp)))
            else update_entry bp;
            if (not (is_high_confidence (List.nth_exn s bp))) && bp < List.length hits
            then update_entry (bp + 1);
            ()
          in
          let _allocate =
            let allocate =
              prediction (List.nth_exn s bp) <> if resolved_direction then 1 else 0
            in
            if allocate
               && !random3 % 16
                  >= !cat * 16 / (Params.controlled_allocation_throttler_max + 1)
            then (
              Stdio.print_endline "allocate";
              let i =
                if List.length hits > 0 then List.hd_exn hits else Params.num_banks
              in
              Stdio.print_s [%message (i : int)];
              let i =
                ref
                  (i
                   - (!random4
                      % (1
                         + (diff (List.hd_exn s)
                            * Params.max_banks_skipped_on_allocation
                            / Params.max_counter_value))))
              in
              Stdio.print_s [%message (!i : int) (diff (List.hd_exn s) : int)];
              let mhc = ref 0 in
              i := !i - 1;
              while !i >= 0 do
                if is_high_confidence result_entries.(!i)
                then (
                  if !random5 % 4 = 0
                  then Array.set result_entries !i (decay result_entries.(!i));
                  if not (is_very_high_confidence result_entries.(!i))
                  then mhc := !mhc + 1;
                  Stdio.print_s [%message "" ~decay_bank:(!i : int) (!mhc : int)];
                  i := !i - 1;
                  ())
                else (
                  Stdio.print_s [%message "" ~allocation_bank:(!i : int)];
                  Array.set
                    result_entries
                    !i
                    (update resolved_direction { num_takens = 0; num_not_takens = 0 });
                  cat := !cat + 2 - (!mhc * 3);
                  cat := min Params.controlled_allocation_throttler_max (max 0 !cat);
                  i := -1;
                  ())
              done;
              ());
            ()
          in
          let _update_randoms =
            let random x =
              let x = x lxor ((x lsl 13) land ((1 lsl 32) - 1)) in
              let x = x lxor (x lsr 17) in
              let x = x lxor ((x lsl 5) land ((1 lsl 32) - 1)) in
              x
            in
            random1 := random !random1;
            random2 := random !random2;
            random3 := random !random3;
            random4 := random !random4;
            random5 := random !random5;
            ()
          in
          ( Array.to_list result_entries
          , (!result_bimodal_direction, !result_bimodal_hysteresis)
          , !meta
          , !cat )
        ;;
      end

      let%expect_test "Model" =
        let open Bits in
        let open Quickcheck in
        sim ~debug:true ~f:(fun _print_state sim ->
          let inputs = Cyclesim.inputs sim in
          let outputs = Cyclesim.outputs sim in
          Cyclesim.reset sim;
          inputs.clear := vdd;
          Cyclesim.cycle sim;
          inputs.clear := gnd;
          Cyclesim.cycle sim;
          let model = Model.init () in
          Quickcheck.test
            (List.quickcheck_generator
               (Generator.tuple6
                  Bool.quickcheck_generator
                  Bool.quickcheck_generator
                  Int.quickcheck_generator
                  Bool.quickcheck_generator
                  (Int.gen_incl 0 (Int.shift_left 1 Params.bimodal_hysteresis_width - 1))
                  (List.gen_with_length
                     Params.num_banks
                     (Generator.tuple3
                        (Int.gen_incl 0 Params.max_counter_value)
                        (Int.gen_incl 0 Params.max_counter_value)
                        Bool.quickcheck_generator))))
            ~sexp_of:
              [%sexp_of: (bool * bool * int * bool * int * (int * int * bool) list) list]
            ~f:(fun updates ->
              List.iter
                updates
                ~f:(fun
                     ( valid
                     , resolved_direction
                     , branch_target
                     , bimodal_direction
                     , bimodal_hysteresis
                     , entries )
                   ->
                inputs.update.valid := of_bool valid;
                inputs.update.resolved_direction := of_bool resolved_direction;
                inputs.update.branch_target
                  := of_int ~width:(width !(inputs.update.branch_target)) branch_target;
                inputs.bimodal_entry.direction := of_bool bimodal_direction;
                inputs.bimodal_entry.hysteresis
                  := of_int
                       ~width:(width !(inputs.bimodal_entry.hysteresis))
                       bimodal_hysteresis;
                List.iter2_exn
                  entries
                  inputs.read_entries
                  ~f:(fun (num_takens, num_not_takens, hit) entry ->
                  entry.tag
                    := if hit
                       then of_int ~width:Params.tag_width 0
                       else of_int ~width:Params.tag_width 1;
                  entry.counters.num_takens
                    := of_int ~width:Params.counter_width num_takens;
                  entry.counters.num_not_takens
                    := of_int ~width:Params.counter_width num_not_takens;
                  ());
                Cyclesim.cycle sim;
                _print_state ();
                if valid
                then (
                  let expected_entries, expected_bimodal, expected_meta, expected_cat =
                    Model.update
                      resolved_direction
                      (List.map entries ~f:(fun (num_takens, num_not_takens, hit) ->
                         { Dual_counter.Model.num_takens; num_not_takens }, hit))
                      (bimodal_direction, bimodal_hysteresis)
                      model
                  in
                  Stdio.print_s [%message "" (model : Model.t)];
                  let { O.next_entries; next_bimodal_entry; meta = _ } =
                    O.map outputs ~f:(( ! ) |> Fn.compose to_int)
                  in
                  [%test_result: Dual_counter.Model.t list * (bool * int) * int * int]
                    ~message:([%message "" (model : Model.t)] |> Sexp.to_string_hum)
                    ~expect:
                      (expected_entries, expected_bimodal, expected_meta, expected_cat)
                    ( List.map
                        next_entries
                        ~f:(fun
                             { Entry.tag = _; counters = { num_takens; num_not_takens } }
                           -> { Dual_counter.Model.num_takens; num_not_takens })
                    , (next_bimodal_entry.direction = 1, next_bimodal_entry.hysteresis)
                    , Cyclesim.internal_ports sim
                      |> List.find_map_exn ~f:(fun (name, s) ->
                           if String.equal name "next_meta"
                           then Some (to_int !s)
                           else None)
                    , Cyclesim.internal_ports sim
                      |> List.find_map_exn ~f:(fun (name, s) ->
                           if String.equal name "next_controlled_allocation_throttler"
                           then Some (to_int !s)
                           else None) );
                  ());
                ());
              ());
          ())
        [@@expect.uncaught_exn
          {|
        (* CR expect_test_collector: This test expectation appears to contain a backtrace.
           This is strongly discouraged as backtraces are fragile.
           Please change this test to not include a backtrace. *)

        ("Base_quickcheck.Test.run: test failed"
          (input
            ((false true 220437019726214 true 0 ((0 3 false) (2 2 false) (0 0 true)))
              (true true 1831639 true 0 ((0 1 false) (3 3 true) (3 0 false)))
              (false true -20566534102334245 true 0
                ((1 2 false) (2 2 true) (0 3 true)))
              (true false 284 true 1 ((3 1 false) (1 3 true) (1 2 false)))
              (false true 635453435 false 0 ((1 2 false) (0 1 true) (0 2 false)))
              (false false 4611686018427387903 true 0
                ((2 3 false) (0 3 false) (1 3 true)))
              (false true 172942876512299687 true 0
                ((1 3 false) (2 0 false) (0 1 true)))))
          (error
            ((runtime-lib/runtime.ml.E
                "(model\
               \n ((meta 0) (cat 14) (random1 2452260707) (random2 997089766)\
               \n  (random3 3864866912) (random4 584905139) (random5 3261080446))): got unexpected result"
               ((expected
                  ((((num_takens 0) (num_not_takens 1))
                     ((num_takens 0) (num_not_takens 3))
                     ((num_takens 1) (num_not_takens 2)))
                    (true 0) 0 14))
                 (got
                   ((((num_takens 3) (num_not_takens 1))
                      ((num_takens 0) (num_not_takens 3))
                      ((num_takens 1) (num_not_takens 2)))
                     (true 0) 0 14))
                 (Loc hard/cpu/lib/branch_prediction.ml:3175:33)))
               "Raised at Ppx_assert_lib__Runtime.test_result in file \"runtime-lib/runtime.ml\", line 106, characters 27-83\
              \nCalled from Cpu__Branch_prediction.BayesianTage.Tests.Update_counters.(fun) in file \"hard/cpu/lib/branch_prediction.ml\", line 3175, characters 33-85\
              \nCalled from Stdlib__List.iter in file \"list.ml\", line 110, characters 12-15\
              \nCalled from Base__List0.iter in file \"src/list0.ml\" (inlined), line 25, characters 16-35\
              \nCalled from Cpu__Branch_prediction.BayesianTage.Tests.Update_counters.(fun) in file \"hard/cpu/lib/branch_prediction.ml\", line 3127, characters 14-1023\
              \nCalled from Base__Or_error.try_with in file \"src/or_error.ml\", line 84, characters 9-15\
              \n")))
        Raised at Base__Error.raise in file "src/error.ml" (inlined), line 9, characters 14-30
        Called from Base__Or_error.ok_exn in file "src/or_error.ml", line 92, characters 17-32
        Called from Core__Quickcheck.Configure.test in file "core/src/quickcheck.ml" (inlined), line 263, characters 4-44
        Called from Cpu__Branch_prediction.BayesianTage.Tests.Update_counters.(fun) in file "hard/cpu/lib/branch_prediction.ml", line 3110, characters 10-1023
        Called from Cpu__Branch_prediction.BayesianTage.Tests.Update_counters.test_bench in file "hard/cpu/lib/branch_prediction.ml", line 2892, characters 8-25
        Called from Cpu__Branch_prediction.BayesianTage.Tests.Update_counters.sim in file "hard/cpu/lib/branch_prediction.ml", line 2900, characters 8-32
        Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

        Trailing output
        ---------------
        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 1) (resolved_direction 1)
             (branch_target 00000000000000011010110001001011)))
           (read_entries
            (((tag 0000000) (counters ((num_takens 11) (num_not_takens 01))))
             ((tag 0000000) (counters ((num_takens 01) (num_not_takens 00))))
             ((tag 0000000) (counters ((num_takens 01) (num_not_takens 00))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 1) (hysteresis 1)))))
         (outputs
          ((next_entries
            (((tag 0000000) (counters ((num_takens 11) (num_not_takens 00))))
             ((tag 0000000) (counters ((num_takens 10) (num_not_takens 00))))
             ((tag 0000000) (counters ((num_takens 10) (num_not_takens 00))))))
           (next_bimodal_entry ((direction 1) (hysteresis 1))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 11) (allocation_bank_mask 111)
           (allocation_decay_range 000) (bank_used_for_prediction 01)
           (controlled_allocation_throttler 0000000) (end_allocation_bank 00)
           (end_allocation_bank_mask 000) (end_allocation_bank_valid 0)
           (first_hitter_bank 00) (first_hitter_mask 011) (gnd 0)
           (hitter_after_prediction_bank 10) (mhc 0)
           (next_controlled_allocation_throttler 0000000) (next_meta 0000)
           (randoms$1 10010010110101101000110010100010)
           (randoms$2 01101110010011011110101011000000)
           (randoms$3 11100100101101101011011001001010)
           (randoms$4 11111111101110010111010101111010)
           (randoms$5 00100100100111101001100000101000) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 0) (zzbank-eq2 0) (zznottakens0 00)
           (zznottakens1 00) (zznottakens2 00) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 01)
           (zztakens1 01) (zztakens2 01))))

        ((bp 1) ("List.nth hits bp" (1)) ("List.length hits" 3)
         (entries
          ((((num_takens 3) (num_not_takens 1)) true)
           (((num_takens 1) (num_not_takens 0)) true)
           (((num_takens 1) (num_not_takens 0)) true))))
        (update_entry (i 1))
        (update_entry (i 2))
        (model
         ((meta 0) (cat 0) (random1 723471715) (random2 2980440586)
          (random3 2105672149) (random4 4091405265) (random5 82288453)))
        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 1) (resolved_direction 0)
             (branch_target 00001011010000001001001100001110)))
           (read_entries
            (((tag 0000001) (counters ((num_takens 00) (num_not_takens 01))))
             ((tag 0000001) (counters ((num_takens 10) (num_not_takens 11))))
             ((tag 0000000) (counters ((num_takens 10) (num_not_takens 01))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 1) (hysteresis 0)))))
         (outputs
          ((next_entries
            (((tag 0000001) (counters ((num_takens 00) (num_not_takens 01))))
             ((tag 0000000) (counters ((num_takens 00) (num_not_takens 01))))
             ((tag 0000000) (counters ((num_takens 10) (num_not_takens 10))))))
           (next_bimodal_entry ((direction 0) (hysteresis 0))) (meta 0000)))
         (internals
          ((allocate 1) (allocation_bank 01) (allocation_bank_mask 001)
           (allocation_decay_range 000) (bank_used_for_prediction 11)
           (controlled_allocation_throttler 0000000) (end_allocation_bank 10)
           (end_allocation_bank_mask 110) (end_allocation_bank_valid 1)
           (first_hitter_bank 10) (first_hitter_mask 110) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0000010) (next_meta 0000)
           (randoms$1 00101011000111110100110101100011)
           (randoms$2 10110001101001011110101000001010)
           (randoms$3 01111101100000100000000111010101)
           (randoms$4 11110011110111011110001111010001)
           (randoms$5 00000100111001111001111101000101) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 1) (zzbank-eq2 0) (zznottakens0 01)
           (zznottakens1 01) (zznottakens2 01) (zzreallyallocate 1)
           (zzreallyallocate_0 1) (zzreallyallocate_1 1) (zztakens0 00)
           (zztakens1 00) (zztakens2 00))))

        ((bp 1) ("List.nth hits bp" ()) ("List.length hits" 1)
         (entries
          ((((num_takens 0) (num_not_takens 1)) false)
           (((num_takens 2) (num_not_takens 3)) false)
           (((num_takens 2) (num_not_takens 1)) true))))
        (update_entry (i 1))
        allocate
        (i 2)
        ((!i 2) ("diff (List.hd_exn s)" 1))
        (allocation_bank 1)
        (model
         ((meta 0) (cat 2) (random1 2497366906) (random2 2423333752)
          (random3 2326748713) (random4 3143328834) (random5 384294018)))
        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 1) (resolved_direction 0)
             (branch_target 00000000000000000000000000000000)))
           (read_entries
            (((tag 0000001) (counters ((num_takens 00) (num_not_takens 00))))
             ((tag 0000001) (counters ((num_takens 01) (num_not_takens 10))))
             ((tag 0000000) (counters ((num_takens 10) (num_not_takens 10))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 0) (hysteresis 0)))))
         (outputs
          ((next_entries
            (((tag 0000001) (counters ((num_takens 00) (num_not_takens 00))))
             ((tag 0000001) (counters ((num_takens 01) (num_not_takens 10))))
             ((tag 0000000) (counters ((num_takens 10) (num_not_takens 11))))))
           (next_bimodal_entry ((direction 0) (hysteresis 1))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 01) (allocation_bank_mask 001)
           (allocation_decay_range 000) (bank_used_for_prediction 11)
           (controlled_allocation_throttler 0000010) (end_allocation_bank 10)
           (end_allocation_bank_mask 110) (end_allocation_bank_valid 1)
           (first_hitter_bank 10) (first_hitter_mask 110) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0000010) (next_meta 0000)
           (randoms$1 10010100110110101100101101111010)
           (randoms$2 10010000011100010010001101111000)
           (randoms$3 10001010101011110101111000101001)
           (randoms$4 10111011010110110110010001000010)
           (randoms$5 00010110111001111101110010000010) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 1) (zzbank-eq2 0) (zznottakens0 01)
           (zznottakens1 01) (zznottakens2 01) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 00)
           (zztakens1 00) (zztakens2 00))))

        ((bp 1) ("List.nth hits bp" ()) ("List.length hits" 1)
         (entries
          ((((num_takens 0) (num_not_takens 0)) false)
           (((num_takens 1) (num_not_takens 2)) false)
           (((num_takens 2) (num_not_takens 2)) true))))
        (update_entry (i 1))
        (model
         ((meta 0) (cat 2) (random1 2064144800) (random2 936466327)
          (random3 1277402396) (random4 767212235) (random5 1133161753)))
        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 1) (resolved_direction 1)
             (branch_target 11111110000001111100100000010001)))
           (read_entries
            (((tag 0000001) (counters ((num_takens 00) (num_not_takens 11))))
             ((tag 0000000) (counters ((num_takens 01) (num_not_takens 01))))
             ((tag 0000001) (counters ((num_takens 00) (num_not_takens 11))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 1) (hysteresis 1)))))
         (outputs
          ((next_entries
            (((tag 0000001) (counters ((num_takens 00) (num_not_takens 11))))
             ((tag 0000000) (counters ((num_takens 10) (num_not_takens 01))))
             ((tag 0000001) (counters ((num_takens 00) (num_not_takens 11))))))
           (next_bimodal_entry ((direction 1) (hysteresis 1))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 11) (allocation_bank_mask 111)
           (allocation_decay_range 100) (bank_used_for_prediction 11)
           (controlled_allocation_throttler 0000010) (end_allocation_bank 01)
           (end_allocation_bank_mask 100) (end_allocation_bank_valid 1)
           (first_hitter_bank 01) (first_hitter_mask 101) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0000010) (next_meta 0000)
           (randoms$1 01111011000010000101100110100000)
           (randoms$2 00110111110100010101011110010111)
           (randoms$3 01001100001000111001110100011100)
           (randoms$4 00101101101110101011101011001011)
           (randoms$5 01000011100010101010110100011001) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 0) (zzbank-eq2 0) (zznottakens0 00)
           (zznottakens1 00) (zznottakens2 00) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 01)
           (zztakens1 01) (zztakens2 01))))

        ((bp 1) ("List.nth hits bp" ()) ("List.length hits" 1)
         (entries
          ((((num_takens 0) (num_not_takens 3)) false)
           (((num_takens 1) (num_not_takens 1)) true)
           (((num_takens 0) (num_not_takens 3)) false))))
        (update_entry (i 1))
        (model
         ((meta 0) (cat 2) (random1 2008045182) (random2 3109329350)
          (random3 3481295196) (random4 647958778) (random5 3541649325)))
        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 1) (resolved_direction 0)
             (branch_target 01101111100010111111001100001001)))
           (read_entries
            (((tag 0000001) (counters ((num_takens 11) (num_not_takens 00))))
             ((tag 0000001) (counters ((num_takens 10) (num_not_takens 01))))
             ((tag 0000001) (counters ((num_takens 10) (num_not_takens 11))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 0) (hysteresis 1)))))
         (outputs
          ((next_entries
            (((tag 0000001) (counters ((num_takens 11) (num_not_takens 00))))
             ((tag 0000001) (counters ((num_takens 10) (num_not_takens 01))))
             ((tag 0000001) (counters ((num_takens 10) (num_not_takens 11))))))
           (next_bimodal_entry ((direction 0) (hysteresis 1))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 10) (allocation_bank_mask 000)
           (allocation_decay_range 000) (bank_used_for_prediction 11)
           (controlled_allocation_throttler 0000010) (end_allocation_bank 11)
           (end_allocation_bank_mask 111) (end_allocation_bank_valid 1)
           (first_hitter_bank 11) (first_hitter_mask 111) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0000010) (next_meta 0000)
           (randoms$1 01110111101100000101011001111110)
           (randoms$2 10111001010101001001100111000110)
           (randoms$3 11001111100000000101100101011100)
           (randoms$4 00100110100111110001000011111010)
           (randoms$5 11010011000110010100011110101101) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 0) (zzbank-eq2 1) (zznottakens0 01)
           (zznottakens1 01) (zznottakens2 01) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 00)
           (zztakens1 00) (zztakens2 00))))

        ((bp 0) ("List.nth hits bp" ()) ("List.length hits" 0)
         (entries
          ((((num_takens 3) (num_not_takens 0)) false)
           (((num_takens 2) (num_not_takens 1)) false)
           (((num_takens 2) (num_not_takens 3)) false))))
        (update_entry (i 0))
        (model
         ((meta 0) (cat 2) (random1 3532304609) (random2 1743114992)
          (random3 1373428265) (random4 1418094074) (random5 2256523579)))
        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 0) (resolved_direction 1)
             (branch_target 11111111000010110111101011000010)))
           (read_entries
            (((tag 0000000) (counters ((num_takens 00) (num_not_takens 00))))
             ((tag 0000001) (counters ((num_takens 01) (num_not_takens 01))))
             ((tag 0000000) (counters ((num_takens 11) (num_not_takens 10))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 0) (hysteresis 0)))))
         (outputs
          ((next_entries
            (((tag 0000000) (counters ((num_takens 01) (num_not_takens 00))))
             ((tag 0000001) (counters ((num_takens 01) (num_not_takens 01))))
             ((tag 0000000) (counters ((num_takens 11) (num_not_takens 01))))))
           (next_bimodal_entry ((direction 1) (hysteresis 0))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 11) (allocation_bank_mask 111)
           (allocation_decay_range 000) (bank_used_for_prediction 11)
           (controlled_allocation_throttler 0000010) (end_allocation_bank 00)
           (end_allocation_bank_mask 000) (end_allocation_bank_valid 0)
           (first_hitter_bank 00) (first_hitter_mask 011) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0000010) (next_meta 0000)
           (randoms$1 11010010100010101011000011100001)
           (randoms$2 01100111111001011101001011110000)
           (randoms$3 01010001110111001101101000101001)
           (randoms$4 01010100100001100110010111111010)
           (randoms$5 10000110011111111101000100111011) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 0) (zzbank-eq2 0) (zznottakens0 00)
           (zznottakens1 00) (zznottakens2 00) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 01)
           (zztakens1 01) (zztakens2 01))))

        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 1) (resolved_direction 1)
             (branch_target 11111111111111111111111000000011)))
           (read_entries
            (((tag 0000000) (counters ((num_takens 01) (num_not_takens 11))))
             ((tag 0000000) (counters ((num_takens 00) (num_not_takens 00))))
             ((tag 0000000) (counters ((num_takens 10) (num_not_takens 01))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 0) (hysteresis 0)))))
         (outputs
          ((next_entries
            (((tag 0000000) (counters ((num_takens 10) (num_not_takens 11))))
             ((tag 0000000) (counters ((num_takens 01) (num_not_takens 00))))
             ((tag 0000000) (counters ((num_takens 11) (num_not_takens 01))))))
           (next_bimodal_entry ((direction 1) (hysteresis 0))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 11) (allocation_bank_mask 111)
           (allocation_decay_range 000) (bank_used_for_prediction 11)
           (controlled_allocation_throttler 0000010) (end_allocation_bank 00)
           (end_allocation_bank_mask 000) (end_allocation_bank_valid 0)
           (first_hitter_bank 00) (first_hitter_mask 011) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0000010) (next_meta 0000)
           (randoms$1 11010010100010101011000011100001)
           (randoms$2 01100111111001011101001011110000)
           (randoms$3 01010001110111001101101000101001)
           (randoms$4 01010100100001100110010111111010)
           (randoms$5 10000110011111111101000100111011) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 0) (zzbank-eq2 0) (zznottakens0 00)
           (zznottakens1 00) (zznottakens2 00) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 01)
           (zztakens1 01) (zztakens2 01))))

        ((bp 3) ("List.nth hits bp" ()) ("List.length hits" 3)
         (entries
          ((((num_takens 1) (num_not_takens 3)) true)
           (((num_takens 0) (num_not_takens 0)) true)
           (((num_takens 2) (num_not_takens 1)) true))))
        (update_entry (i 3))
        allocate
        (i 0)
        ((!i 0) ("diff (List.hd_exn s)" 2))
        (model
         ((meta 0) (cat 2) (random1 374114282) (random2 1791768973)
          (random3 2578084805) (random4 2668909862) (random5 4148784631)))
        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 1) (resolved_direction 0)
             (branch_target 00111100010101111111000001000000)))
           (read_entries
            (((tag 0000001) (counters ((num_takens 10) (num_not_takens 00))))
             ((tag 0000001) (counters ((num_takens 01) (num_not_takens 01))))
             ((tag 0000000) (counters ((num_takens 00) (num_not_takens 11))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 0) (hysteresis 0)))))
         (outputs
          ((next_entries
            (((tag 0000001) (counters ((num_takens 10) (num_not_takens 00))))
             ((tag 0000001) (counters ((num_takens 01) (num_not_takens 01))))
             ((tag 0000000) (counters ((num_takens 00) (num_not_takens 11))))))
           (next_bimodal_entry ((direction 0) (hysteresis 0))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 01) (allocation_bank_mask 001)
           (allocation_decay_range 000) (bank_used_for_prediction 10)
           (controlled_allocation_throttler 0000010) (end_allocation_bank 10)
           (end_allocation_bank_mask 110) (end_allocation_bank_valid 1)
           (first_hitter_bank 10) (first_hitter_mask 110) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0000010) (next_meta 0000)
           (randoms$1 00010110010011001000011111101010)
           (randoms$2 01101010110011000011100110001101)
           (randoms$3 10011001101010100111001111000101)
           (randoms$4 10011111000101000101010100100110)
           (randoms$5 11110111010010010110110111110111) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 1) (zzbank-eq2 0) (zznottakens0 01)
           (zznottakens1 01) (zznottakens2 01) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 00)
           (zztakens1 00) (zztakens2 00))))

        ((bp 0) ("List.nth hits bp" (2)) ("List.length hits" 1)
         (entries
          ((((num_takens 2) (num_not_takens 0)) false)
           (((num_takens 1) (num_not_takens 1)) false)
           (((num_takens 0) (num_not_takens 3)) true))))
        inside
        (model
         ((meta 0) (cat 2) (random1 1350636274) (random2 1379926291)
          (random3 763739564) (random4 2745393214) (random5 2215375116)))
        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 0) (resolved_direction 1)
             (branch_target 00000011101100001011001011001000)))
           (read_entries
            (((tag 0000000) (counters ((num_takens 10) (num_not_takens 11))))
             ((tag 0000000) (counters ((num_takens 00) (num_not_takens 01))))
             ((tag 0000001) (counters ((num_takens 01) (num_not_takens 00))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 0) (hysteresis 1)))))
         (outputs
          ((next_entries
            (((tag 0000000) (counters ((num_takens 11) (num_not_takens 11))))
             ((tag 0000000) (counters ((num_takens 01) (num_not_takens 01))))
             ((tag 0000001) (counters ((num_takens 01) (num_not_takens 00))))))
           (next_bimodal_entry ((direction 0) (hysteresis 0))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 11) (allocation_bank_mask 111)
           (allocation_decay_range 000) (bank_used_for_prediction 01)
           (controlled_allocation_throttler 0000010) (end_allocation_bank 00)
           (end_allocation_bank_mask 000) (end_allocation_bank_valid 0)
           (first_hitter_bank 00) (first_hitter_mask 011) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0000010) (next_meta 0000)
           (randoms$1 01010000100000010001001011110010)
           (randoms$2 01010010010000000000000100010011)
           (randoms$3 00101101100001011011110110101100)
           (randoms$4 10100011101000110110000000111110)
           (randoms$5 10000100000010111111000100001100) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 0) (zzbank-eq2 0) (zznottakens0 00)
           (zznottakens1 00) (zznottakens2 00) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 01)
           (zztakens1 01) (zztakens2 01))))

        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 0) (resolved_direction 1)
             (branch_target 00000000000000000000000000001001)))
           (read_entries
            (((tag 0000001) (counters ((num_takens 00) (num_not_takens 10))))
             ((tag 0000000) (counters ((num_takens 11) (num_not_takens 00))))
             ((tag 0000001) (counters ((num_takens 11) (num_not_takens 10))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 1) (hysteresis 0)))))
         (outputs
          ((next_entries
            (((tag 0000001) (counters ((num_takens 00) (num_not_takens 10))))
             ((tag 0000000) (counters ((num_takens 11) (num_not_takens 00))))
             ((tag 0000001) (counters ((num_takens 11) (num_not_takens 10))))))
           (next_bimodal_entry ((direction 1) (hysteresis 0))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 11) (allocation_bank_mask 111)
           (allocation_decay_range 100) (bank_used_for_prediction 01)
           (controlled_allocation_throttler 0000010) (end_allocation_bank 01)
           (end_allocation_bank_mask 100) (end_allocation_bank_valid 1)
           (first_hitter_bank 01) (first_hitter_mask 101) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0000010) (next_meta 0000)
           (randoms$1 01010000100000010001001011110010)
           (randoms$2 01010010010000000000000100010011)
           (randoms$3 00101101100001011011110110101100)
           (randoms$4 10100011101000110110000000111110)
           (randoms$5 10000100000010111111000100001100) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 0) (zzbank-eq2 0) (zznottakens0 00)
           (zznottakens1 00) (zznottakens2 00) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 01)
           (zztakens1 01) (zztakens2 01))))

        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 0) (resolved_direction 1)
             (branch_target 11111111111111111111111111110101)))
           (read_entries
            (((tag 0000000) (counters ((num_takens 00) (num_not_takens 11))))
             ((tag 0000000) (counters ((num_takens 10) (num_not_takens 00))))
             ((tag 0000001) (counters ((num_takens 01) (num_not_takens 00))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 0) (hysteresis 1)))))
         (outputs
          ((next_entries
            (((tag 0000000) (counters ((num_takens 01) (num_not_takens 11))))
             ((tag 0000000) (counters ((num_takens 10) (num_not_takens 00))))
             ((tag 0000001) (counters ((num_takens 01) (num_not_takens 00))))))
           (next_bimodal_entry ((direction 0) (hysteresis 1))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 11) (allocation_bank_mask 111)
           (allocation_decay_range 000) (bank_used_for_prediction 00)
           (controlled_allocation_throttler 0000010) (end_allocation_bank 00)
           (end_allocation_bank_mask 000) (end_allocation_bank_valid 0)
           (first_hitter_bank 00) (first_hitter_mask 011) (gnd 0)
           (hitter_after_prediction_bank 01) (mhc 0)
           (next_controlled_allocation_throttler 0000010) (next_meta 0000)
           (randoms$1 01010000100000010001001011110010)
           (randoms$2 01010010010000000000000100010011)
           (randoms$3 00101101100001011011110110101100)
           (randoms$4 10100011101000110110000000111110)
           (randoms$5 10000100000010111111000100001100) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 0) (zzbank-eq2 0) (zznottakens0 00)
           (zznottakens1 00) (zznottakens2 00) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 01)
           (zztakens1 01) (zztakens2 01))))

        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 1) (resolved_direction 1)
             (branch_target 11111111111111111100000111000011)))
           (read_entries
            (((tag 0000001) (counters ((num_takens 10) (num_not_takens 01))))
             ((tag 0000001) (counters ((num_takens 11) (num_not_takens 00))))
             ((tag 0000001) (counters ((num_takens 11) (num_not_takens 11))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 0) (hysteresis 0)))))
         (outputs
          ((next_entries
            (((tag 0000001) (counters ((num_takens 10) (num_not_takens 01))))
             ((tag 0000001) (counters ((num_takens 11) (num_not_takens 00))))
             ((tag 0000000) (counters ((num_takens 01) (num_not_takens 00))))))
           (next_bimodal_entry ((direction 1) (hysteresis 0))) (meta 0000)))
         (internals
          ((allocate 1) (allocation_bank 10) (allocation_bank_mask 000)
           (allocation_decay_range 000) (bank_used_for_prediction 11)
           (controlled_allocation_throttler 0000010) (end_allocation_bank 11)
           (end_allocation_bank_mask 111) (end_allocation_bank_valid 1)
           (first_hitter_bank 11) (first_hitter_mask 111) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0000100) (next_meta 0000)
           (randoms$1 01010000100000010001001011110010)
           (randoms$2 01010010010000000000000100010011)
           (randoms$3 00101101100001011011110110101100)
           (randoms$4 10100011101000110110000000111110)
           (randoms$5 10000100000010111111000100001100) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 0) (zzbank-eq2 1) (zznottakens0 00)
           (zznottakens1 00) (zznottakens2 00) (zzreallyallocate 1)
           (zzreallyallocate_0 1) (zzreallyallocate_1 1) (zztakens0 01)
           (zztakens1 01) (zztakens2 01))))

        ((bp 0) ("List.nth hits bp" ()) ("List.length hits" 0)
         (entries
          ((((num_takens 2) (num_not_takens 1)) false)
           (((num_takens 3) (num_not_takens 0)) false)
           (((num_takens 3) (num_not_takens 3)) false))))
        (update_entry (i 0))
        allocate
        (i 3)
        ((!i 3) ("diff (List.hd_exn s)" 2))
        (allocation_bank 2)
        (model
         ((meta 0) (cat 4) (random1 691148861) (random2 506154082)
          (random3 3695076916) (random4 993802860) (random5 3211497273)))
        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 1) (resolved_direction 1)
             (branch_target 00000000000000000011001001011011)))
           (read_entries
            (((tag 0000000) (counters ((num_takens 01) (num_not_takens 11))))
             ((tag 0000000) (counters ((num_takens 01) (num_not_takens 00))))
             ((tag 0000001) (counters ((num_takens 01) (num_not_takens 01))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 1) (hysteresis 0)))))
         (outputs
          ((next_entries
            (((tag 0000000) (counters ((num_takens 10) (num_not_takens 11))))
             ((tag 0000000) (counters ((num_takens 10) (num_not_takens 00))))
             ((tag 0000001) (counters ((num_takens 01) (num_not_takens 01))))))
           (next_bimodal_entry ((direction 1) (hysteresis 1))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 11) (allocation_bank_mask 111)
           (allocation_decay_range 000) (bank_used_for_prediction 01)
           (controlled_allocation_throttler 0000100) (end_allocation_bank 00)
           (end_allocation_bank_mask 000) (end_allocation_bank_valid 0)
           (first_hitter_bank 00) (first_hitter_mask 011) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0000100) (next_meta 0000)
           (randoms$1 00101001001100100001100000111101)
           (randoms$2 00011110001010110100110001100010)
           (randoms$3 11011100001111100110011000110100)
           (randoms$4 00111011001111000011101001101100)
           (randoms$5 10111111011010111000111100111001) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 0) (zzbank-eq2 0) (zznottakens0 00)
           (zznottakens1 00) (zznottakens2 00) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 01)
           (zztakens1 01) (zztakens2 01))))

        ((bp 1) ("List.nth hits bp" (1)) ("List.length hits" 2)
         (entries
          ((((num_takens 1) (num_not_takens 3)) true)
           (((num_takens 1) (num_not_takens 0)) true)
           (((num_takens 1) (num_not_takens 1)) false))))
        (update_entry (i 1))
        (update_entry (i 2))
        (model
         ((meta 0) (cat 4) (random1 746858951) (random2 2202124689)
          (random3 266676040) (random4 843935444) (random5 521521055)))
        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 1) (resolved_direction 1)
             (branch_target 11101011110000110011010110000111)))
           (read_entries
            (((tag 0000001) (counters ((num_takens 01) (num_not_takens 11))))
             ((tag 0000000) (counters ((num_takens 11) (num_not_takens 10))))
             ((tag 0000000) (counters ((num_takens 00) (num_not_takens 10))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 0) (hysteresis 1)))))
         (outputs
          ((next_entries
            (((tag 0000000) (counters ((num_takens 01) (num_not_takens 00))))
             ((tag 0000000) (counters ((num_takens 11) (num_not_takens 01))))
             ((tag 0000000) (counters ((num_takens 01) (num_not_takens 10))))))
           (next_bimodal_entry ((direction 0) (hysteresis 1))) (meta 0000)))
         (internals
          ((allocate 1) (allocation_bank 00) (allocation_bank_mask 011)
           (allocation_decay_range 000) (bank_used_for_prediction 10)
           (controlled_allocation_throttler 0000100) (end_allocation_bank 01)
           (end_allocation_bank_mask 100) (end_allocation_bank_valid 1)
           (first_hitter_bank 01) (first_hitter_mask 101) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0000110) (next_meta 0000)
           (randoms$1 00101100100001000010100111000111)
           (randoms$2 10000011010000011100000110010001)
           (randoms$3 00001111111001010010011101001000)
           (randoms$4 00110010010011010110111011010100)
           (randoms$5 00011111000101011100011110011111) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 1) (zzbank-eq1 0) (zzbank-eq2 0) (zznottakens0 00)
           (zznottakens1 00) (zznottakens2 00) (zzreallyallocate 1)
           (zzreallyallocate_0 1) (zzreallyallocate_1 1) (zztakens0 01)
           (zztakens1 01) (zztakens2 01))))

        ((bp 1) ("List.nth hits bp" (2)) ("List.length hits" 2)
         (entries
          ((((num_takens 1) (num_not_takens 3)) false)
           (((num_takens 3) (num_not_takens 2)) true)
           (((num_takens 0) (num_not_takens 2)) true))))
        (update_entry (i 1))
        allocate
        (i 1)
        ((!i 1) ("diff (List.hd_exn s)" 1))
        (allocation_bank 0)
        (model
         ((meta 0) (cat 6) (random1 2653896249) (random2 3573823784)
          (random3 3397528334) (random4 1835238143) (random5 1529412076)))
        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 0) (resolved_direction 1)
             (branch_target 00010110011101101100010000110001)))
           (read_entries
            (((tag 0000001) (counters ((num_takens 10) (num_not_takens 00))))
             ((tag 0000000) (counters ((num_takens 01) (num_not_takens 10))))
             ((tag 0000001) (counters ((num_takens 11) (num_not_takens 01))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 1) (hysteresis 1)))))
         (outputs
          ((next_entries
            (((tag 0000001) (counters ((num_takens 10) (num_not_takens 00))))
             ((tag 0000000) (counters ((num_takens 10) (num_not_takens 10))))
             ((tag 0000001) (counters ((num_takens 11) (num_not_takens 01))))))
           (next_bimodal_entry ((direction 1) (hysteresis 1))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 11) (allocation_bank_mask 111)
           (allocation_decay_range 100) (bank_used_for_prediction 11)
           (controlled_allocation_throttler 0000110) (end_allocation_bank 01)
           (end_allocation_bank_mask 100) (end_allocation_bank_valid 1)
           (first_hitter_bank 01) (first_hitter_mask 101) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0000110) (next_meta 0000)
           (randoms$1 10011110001011110011111000111001)
           (randoms$2 11010101000001000011100100101000)
           (randoms$3 11001010100000100010101100001110)
           (randoms$4 01101101011000111000001011111111)
           (randoms$5 01011011001010001111100111101100) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 0) (zzbank-eq2 0) (zznottakens0 00)
           (zznottakens1 00) (zznottakens2 00) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 01)
           (zztakens1 01) (zztakens2 01))))

        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 0) (resolved_direction 1)
             (branch_target 11111111111111111111111111111011)))
           (read_entries
            (((tag 0000000) (counters ((num_takens 11) (num_not_takens 11))))
             ((tag 0000001) (counters ((num_takens 10) (num_not_takens 11))))
             ((tag 0000001) (counters ((num_takens 11) (num_not_takens 10))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 0) (hysteresis 0)))))
         (outputs
          ((next_entries
            (((tag 0000000) (counters ((num_takens 11) (num_not_takens 10))))
             ((tag 0000001) (counters ((num_takens 10) (num_not_takens 11))))
             ((tag 0000001) (counters ((num_takens 11) (num_not_takens 10))))))
           (next_bimodal_entry ((direction 1) (hysteresis 0))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 11) (allocation_bank_mask 111)
           (allocation_decay_range 000) (bank_used_for_prediction 11)
           (controlled_allocation_throttler 0000110) (end_allocation_bank 00)
           (end_allocation_bank_mask 000) (end_allocation_bank_valid 0)
           (first_hitter_bank 00) (first_hitter_mask 011) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0000110) (next_meta 0000)
           (randoms$1 10011110001011110011111000111001)
           (randoms$2 11010101000001000011100100101000)
           (randoms$3 11001010100000100010101100001110)
           (randoms$4 01101101011000111000001011111111)
           (randoms$5 01011011001010001111100111101100) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 0) (zzbank-eq2 0) (zznottakens0 00)
           (zznottakens1 00) (zznottakens2 00) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 01)
           (zztakens1 01) (zztakens2 01))))

        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 0) (resolved_direction 1)
             (branch_target 10111000001111110100001101000110)))
           (read_entries
            (((tag 0000000) (counters ((num_takens 00) (num_not_takens 10))))
             ((tag 0000000) (counters ((num_takens 11) (num_not_takens 11))))
             ((tag 0000001) (counters ((num_takens 10) (num_not_takens 11))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 1) (hysteresis 0)))))
         (outputs
          ((next_entries
            (((tag 0000000) (counters ((num_takens 01) (num_not_takens 10))))
             ((tag 0000000) (counters ((num_takens 11) (num_not_takens 11))))
             ((tag 0000001) (counters ((num_takens 10) (num_not_takens 11))))))
           (next_bimodal_entry ((direction 1) (hysteresis 0))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 11) (allocation_bank_mask 111)
           (allocation_decay_range 000) (bank_used_for_prediction 00)
           (controlled_allocation_throttler 0000110) (end_allocation_bank 00)
           (end_allocation_bank_mask 000) (end_allocation_bank_valid 0)
           (first_hitter_bank 00) (first_hitter_mask 011) (gnd 0)
           (hitter_after_prediction_bank 01) (mhc 0)
           (next_controlled_allocation_throttler 0000110) (next_meta 0000)
           (randoms$1 10011110001011110011111000111001)
           (randoms$2 11010101000001000011100100101000)
           (randoms$3 11001010100000100010101100001110)
           (randoms$4 01101101011000111000001011111111)
           (randoms$5 01011011001010001111100111101100) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 0) (zzbank-eq2 0) (zznottakens0 00)
           (zznottakens1 00) (zznottakens2 00) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 01)
           (zztakens1 01) (zztakens2 01))))

        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 1) (resolved_direction 1)
             (branch_target 00000000000000000000000000001000)))
           (read_entries
            (((tag 0000000) (counters ((num_takens 10) (num_not_takens 10))))
             ((tag 0000000) (counters ((num_takens 00) (num_not_takens 11))))
             ((tag 0000000) (counters ((num_takens 01) (num_not_takens 10))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 1) (hysteresis 0)))))
         (outputs
          ((next_entries
            (((tag 0000000) (counters ((num_takens 11) (num_not_takens 10))))
             ((tag 0000000) (counters ((num_takens 01) (num_not_takens 11))))
             ((tag 0000000) (counters ((num_takens 01) (num_not_takens 10))))))
           (next_bimodal_entry ((direction 1) (hysteresis 0))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 11) (allocation_bank_mask 111)
           (allocation_decay_range 000) (bank_used_for_prediction 01)
           (controlled_allocation_throttler 0000110) (end_allocation_bank 00)
           (end_allocation_bank_mask 000) (end_allocation_bank_valid 0)
           (first_hitter_bank 00) (first_hitter_mask 011) (gnd 0)
           (hitter_after_prediction_bank 10) (mhc 0)
           (next_controlled_allocation_throttler 0000110) (next_meta 0000)
           (randoms$1 10011110001011110011111000111001)
           (randoms$2 11010101000001000011100100101000)
           (randoms$3 11001010100000100010101100001110)
           (randoms$4 01101101011000111000001011111111)
           (randoms$5 01011011001010001111100111101100) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 0) (zzbank-eq2 0) (zznottakens0 00)
           (zznottakens1 00) (zznottakens2 00) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 01)
           (zztakens1 01) (zztakens2 01))))

        ((bp 1) ("List.nth hits bp" (1)) ("List.length hits" 3)
         (entries
          ((((num_takens 2) (num_not_takens 2)) true)
           (((num_takens 0) (num_not_takens 3)) true)
           (((num_takens 1) (num_not_takens 2)) true))))
        (update_entry (i 1))
        allocate
        (i 0)
        ((!i 0) ("diff (List.hd_exn s)" 0))
        (model
         ((meta 0) (cat 6) (random1 1156348781) (random2 369301304)
          (random3 1939223327) (random4 3132219457) (random5 3334350630)))
        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 0) (resolved_direction 0)
             (branch_target 11111111111111111111111111111001)))
           (read_entries
            (((tag 0000000) (counters ((num_takens 01) (num_not_takens 01))))
             ((tag 0000000) (counters ((num_takens 10) (num_not_takens 00))))
             ((tag 0000000) (counters ((num_takens 00) (num_not_takens 00))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 1) (hysteresis 1)))))
         (outputs
          ((next_entries
            (((tag 0000000) (counters ((num_takens 01) (num_not_takens 10))))
             ((tag 0000000) (counters ((num_takens 10) (num_not_takens 01))))
             ((tag 0000000) (counters ((num_takens 00) (num_not_takens 00))))))
           (next_bimodal_entry ((direction 1) (hysteresis 1))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 11) (allocation_bank_mask 111)
           (allocation_decay_range 000) (bank_used_for_prediction 01)
           (controlled_allocation_throttler 0000110) (end_allocation_bank 00)
           (end_allocation_bank_mask 000) (end_allocation_bank_valid 0)
           (first_hitter_bank 00) (first_hitter_mask 011) (gnd 0)
           (hitter_after_prediction_bank 10) (mhc 0)
           (next_controlled_allocation_throttler 0000110) (next_meta 0000)
           (randoms$1 01000100111011000111101101101101)
           (randoms$2 00010110000000110001011100111000)
           (randoms$3 01110011100101100011001100011111)
           (randoms$4 10111010101100011110000001000001)
           (randoms$5 11000110101111100010011100100110) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 0) (zzbank-eq2 0) (zznottakens0 01)
           (zznottakens1 01) (zznottakens2 01) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 00)
           (zztakens1 00) (zztakens2 00))))

        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 0) (resolved_direction 1)
             (branch_target 11111111111111111111111110001101)))
           (read_entries
            (((tag 0000000) (counters ((num_takens 01) (num_not_takens 10))))
             ((tag 0000000) (counters ((num_takens 00) (num_not_takens 11))))
             ((tag 0000001) (counters ((num_takens 10) (num_not_takens 11))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 0) (hysteresis 1)))))
         (outputs
          ((next_entries
            (((tag 0000000) (counters ((num_takens 10) (num_not_takens 10))))
             ((tag 0000000) (counters ((num_takens 01) (num_not_takens 11))))
             ((tag 0000001) (counters ((num_takens 10) (num_not_takens 11))))))
           (next_bimodal_entry ((direction 0) (hysteresis 1))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 11) (allocation_bank_mask 111)
           (allocation_decay_range 000) (bank_used_for_prediction 01)
           (controlled_allocation_throttler 0000110) (end_allocation_bank 00)
           (end_allocation_bank_mask 000) (end_allocation_bank_valid 0)
           (first_hitter_bank 00) (first_hitter_mask 011) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0000110) (next_meta 0000)
           (randoms$1 01000100111011000111101101101101)
           (randoms$2 00010110000000110001011100111000)
           (randoms$3 01110011100101100011001100011111)
           (randoms$4 10111010101100011110000001000001)
           (randoms$5 11000110101111100010011100100110) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 0) (zzbank-eq2 0) (zznottakens0 00)
           (zznottakens1 00) (zznottakens2 00) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 01)
           (zztakens1 01) (zztakens2 01))))

        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 0) (resolved_direction 1)
             (branch_target 10000011110111101100011001111000)))
           (read_entries
            (((tag 0000001) (counters ((num_takens 00) (num_not_takens 00))))
             ((tag 0000001) (counters ((num_takens 11) (num_not_takens 10))))
             ((tag 0000001) (counters ((num_takens 01) (num_not_takens 10))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 0) (hysteresis 0)))))
         (outputs
          ((next_entries
            (((tag 0000001) (counters ((num_takens 00) (num_not_takens 00))))
             ((tag 0000001) (counters ((num_takens 11) (num_not_takens 10))))
             ((tag 0000000) (counters ((num_takens 01) (num_not_takens 00))))))
           (next_bimodal_entry ((direction 1) (hysteresis 0))) (meta 0000)))
         (internals
          ((allocate 1) (allocation_bank 10) (allocation_bank_mask 000)
           (allocation_decay_range 000) (bank_used_for_prediction 11)
           (controlled_allocation_throttler 0000110) (end_allocation_bank 11)
           (end_allocation_bank_mask 111) (end_allocation_bank_valid 1)
           (first_hitter_bank 11) (first_hitter_mask 111) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0001000) (next_meta 0000)
           (randoms$1 01000100111011000111101101101101)
           (randoms$2 00010110000000110001011100111000)
           (randoms$3 01110011100101100011001100011111)
           (randoms$4 10111010101100011110000001000001)
           (randoms$5 11000110101111100010011100100110) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 0) (zzbank-eq2 1) (zznottakens0 00)
           (zznottakens1 00) (zznottakens2 00) (zzreallyallocate 1)
           (zzreallyallocate_0 1) (zzreallyallocate_1 1) (zztakens0 01)
           (zztakens1 01) (zztakens2 01))))

        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 1) (resolved_direction 0)
             (branch_target 11111111011100010001011000110110)))
           (read_entries
            (((tag 0000001) (counters ((num_takens 01) (num_not_takens 01))))
             ((tag 0000000) (counters ((num_takens 10) (num_not_takens 10))))
             ((tag 0000000) (counters ((num_takens 11) (num_not_takens 11))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 1) (hysteresis 1)))))
         (outputs
          ((next_entries
            (((tag 0000000) (counters ((num_takens 00) (num_not_takens 01))))
             ((tag 0000000) (counters ((num_takens 10) (num_not_takens 11))))
             ((tag 0000000) (counters ((num_takens 10) (num_not_takens 11))))))
           (next_bimodal_entry ((direction 1) (hysteresis 0))) (meta 0000)))
         (internals
          ((allocate 1) (allocation_bank 00) (allocation_bank_mask 011)
           (allocation_decay_range 000) (bank_used_for_prediction 11)
           (controlled_allocation_throttler 0000110) (end_allocation_bank 01)
           (end_allocation_bank_mask 100) (end_allocation_bank_valid 1)
           (first_hitter_bank 01) (first_hitter_mask 101) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0001000) (next_meta 0000)
           (randoms$1 01000100111011000111101101101101)
           (randoms$2 00010110000000110001011100111000)
           (randoms$3 01110011100101100011001100011111)
           (randoms$4 10111010101100011110000001000001)
           (randoms$5 11000110101111100010011100100110) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 1) (zzbank-eq1 0) (zzbank-eq2 0) (zznottakens0 01)
           (zznottakens1 01) (zznottakens2 01) (zzreallyallocate 1)
           (zzreallyallocate_0 1) (zzreallyallocate_1 1) (zztakens0 00)
           (zztakens1 00) (zztakens2 00))))

        ((bp 2) ("List.nth hits bp" ()) ("List.length hits" 2)
         (entries
          ((((num_takens 1) (num_not_takens 1)) false)
           (((num_takens 2) (num_not_takens 2)) true)
           (((num_takens 3) (num_not_takens 3)) true))))
        (update_entry (i 2))
        allocate
        (i 1)
        ((!i 1) ("diff (List.hd_exn s)" 0))
        (allocation_bank 0)
        (model
         ((meta 0) (cat 8) (random1 3149294349) (random2 3898704906)
          (random3 189052229) (random4 1367990461) (random5 1225140075)))
        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 1) (resolved_direction 0)
             (branch_target 11111111111111111111110110111101)))
           (read_entries
            (((tag 0000000) (counters ((num_takens 00) (num_not_takens 01))))
             ((tag 0000000) (counters ((num_takens 00) (num_not_takens 01))))
             ((tag 0000000) (counters ((num_takens 10) (num_not_takens 01))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 0) (hysteresis 0)))))
         (outputs
          ((next_entries
            (((tag 0000000) (counters ((num_takens 00) (num_not_takens 10))))
             ((tag 0000000) (counters ((num_takens 00) (num_not_takens 10))))
             ((tag 0000000) (counters ((num_takens 10) (num_not_takens 01))))))
           (next_bimodal_entry ((direction 0) (hysteresis 0))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 11) (allocation_bank_mask 111)
           (allocation_decay_range 000) (bank_used_for_prediction 00)
           (controlled_allocation_throttler 0001000) (end_allocation_bank 00)
           (end_allocation_bank_mask 000) (end_allocation_bank_valid 0)
           (first_hitter_bank 00) (first_hitter_mask 011) (gnd 0)
           (hitter_after_prediction_bank 01) (mhc 0)
           (next_controlled_allocation_throttler 0001000) (next_meta 0000)
           (randoms$1 10111011101101100110101100001101)
           (randoms$2 11101000011000011000010000001010)
           (randoms$3 00001011010001001011010101000101)
           (randoms$4 01010001100010011110000010111101)
           (randoms$5 01001001000001100010011101101011) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 0) (zzbank-eq2 0) (zznottakens0 01)
           (zznottakens1 01) (zznottakens2 01) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 00)
           (zztakens1 00) (zztakens2 00))))

        ((bp 0) ("List.nth hits bp" (0)) ("List.length hits" 3)
         (entries
          ((((num_takens 0) (num_not_takens 1)) true)
           (((num_takens 0) (num_not_takens 1)) true)
           (((num_takens 2) (num_not_takens 1)) true))))
        (update_entry (i 0))
        (update_entry (i 1))
        (model
         ((meta 0) (cat 8) (random1 2888432806) (random2 3304433466)
          (random3 543632851) (random4 3729831986) (random5 814428766)))
        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 0) (resolved_direction 1)
             (branch_target 11111111111111001111110111000011)))
           (read_entries
            (((tag 0000001) (counters ((num_takens 00) (num_not_takens 11))))
             ((tag 0000001) (counters ((num_takens 10) (num_not_takens 00))))
             ((tag 0000000) (counters ((num_takens 11) (num_not_takens 10))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 1) (hysteresis 1)))))
         (outputs
          ((next_entries
            (((tag 0000001) (counters ((num_takens 00) (num_not_takens 11))))
             ((tag 0000001) (counters ((num_takens 10) (num_not_takens 00))))
             ((tag 0000000) (counters ((num_takens 11) (num_not_takens 01))))))
           (next_bimodal_entry ((direction 1) (hysteresis 1))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 11) (allocation_bank_mask 111)
           (allocation_decay_range 110) (bank_used_for_prediction 11)
           (controlled_allocation_throttler 0001000) (end_allocation_bank 10)
           (end_allocation_bank_mask 110) (end_allocation_bank_valid 1)
           (first_hitter_bank 10) (first_hitter_mask 110) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0001000) (next_meta 0000)
           (randoms$1 10101100001010011111110010100110)
           (randoms$2 11000100111101011010011100111010)
           (randoms$3 00100000011001110010110111010011)
           (randoms$4 11011110010100001011100000110010)
           (randoms$5 00110000100010110011001001011110) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 0) (zzbank-eq2 0) (zznottakens0 00)
           (zznottakens1 00) (zznottakens2 00) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 01)
           (zztakens1 01) (zztakens2 01))))

        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 1) (resolved_direction 0)
             (branch_target 00000000001101111011100011010100)))
           (read_entries
            (((tag 0000000) (counters ((num_takens 11) (num_not_takens 11))))
             ((tag 0000001) (counters ((num_takens 00) (num_not_takens 11))))
             ((tag 0000001) (counters ((num_takens 01) (num_not_takens 01))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 0) (hysteresis 1)))))
         (outputs
          ((next_entries
            (((tag 0000000) (counters ((num_takens 10) (num_not_takens 11))))
             ((tag 0000001) (counters ((num_takens 00) (num_not_takens 11))))
             ((tag 0000001) (counters ((num_takens 01) (num_not_takens 01))))))
           (next_bimodal_entry ((direction 0) (hysteresis 1))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 11) (allocation_bank_mask 111)
           (allocation_decay_range 000) (bank_used_for_prediction 11)
           (controlled_allocation_throttler 0001000) (end_allocation_bank 00)
           (end_allocation_bank_mask 000) (end_allocation_bank_valid 0)
           (first_hitter_bank 00) (first_hitter_mask 011) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0001000) (next_meta 0000)
           (randoms$1 10101100001010011111110010100110)
           (randoms$2 11000100111101011010011100111010)
           (randoms$3 00100000011001110010110111010011)
           (randoms$4 11011110010100001011100000110010)
           (randoms$5 00110000100010110011001001011110) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 0) (zzbank-eq2 0) (zznottakens0 01)
           (zznottakens1 01) (zznottakens2 01) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 00)
           (zztakens1 00) (zztakens2 00))))

        ((bp 1) ("List.nth hits bp" ()) ("List.length hits" 1)
         (entries
          ((((num_takens 3) (num_not_takens 3)) true)
           (((num_takens 0) (num_not_takens 3)) false)
           (((num_takens 1) (num_not_takens 1)) false))))
        (update_entry (i 1))
        (model
         ((meta 0) (cat 8) (random1 3826506360) (random2 1917401427)
          (random3 2121844893) (random4 3817148345) (random5 2396782334)))
        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 0) (resolved_direction 1)
             (branch_target 00111000010110000100110101011011)))
           (read_entries
            (((tag 0000000) (counters ((num_takens 00) (num_not_takens 10))))
             ((tag 0000000) (counters ((num_takens 11) (num_not_takens 01))))
             ((tag 0000001) (counters ((num_takens 00) (num_not_takens 00))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 0) (hysteresis 1)))))
         (outputs
          ((next_entries
            (((tag 0000000) (counters ((num_takens 01) (num_not_takens 10))))
             ((tag 0000000) (counters ((num_takens 11) (num_not_takens 01))))
             ((tag 0000001) (counters ((num_takens 00) (num_not_takens 00))))))
           (next_bimodal_entry ((direction 0) (hysteresis 1))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 11) (allocation_bank_mask 111)
           (allocation_decay_range 000) (bank_used_for_prediction 00)
           (controlled_allocation_throttler 0001000) (end_allocation_bank 00)
           (end_allocation_bank_mask 000) (end_allocation_bank_valid 0)
           (first_hitter_bank 00) (first_hitter_mask 011) (gnd 0)
           (hitter_after_prediction_bank 01) (mhc 0)
           (next_controlled_allocation_throttler 0001000) (next_meta 0000)
           (randoms$1 11100100000100111101101001111000)
           (randoms$2 01110010010010010011100101010011)
           (randoms$3 01111110011110001100100010011101)
           (randoms$4 11100011100001010000111110111001)
           (randoms$5 10001110110110111111111011111110) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 0) (zzbank-eq2 0) (zznottakens0 00)
           (zznottakens1 00) (zznottakens2 00) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 01)
           (zztakens1 01) (zztakens2 01))))

        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 0) (resolved_direction 0)
             (branch_target 00000000000000000001111110011111)))
           (read_entries
            (((tag 0000000) (counters ((num_takens 10) (num_not_takens 11))))
             ((tag 0000001) (counters ((num_takens 00) (num_not_takens 00))))
             ((tag 0000001) (counters ((num_takens 00) (num_not_takens 11))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 1) (hysteresis 0)))))
         (outputs
          ((next_entries
            (((tag 0000000) (counters ((num_takens 01) (num_not_takens 11))))
             ((tag 0000001) (counters ((num_takens 00) (num_not_takens 00))))
             ((tag 0000001) (counters ((num_takens 00) (num_not_takens 11))))))
           (next_bimodal_entry ((direction 0) (hysteresis 0))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 11) (allocation_bank_mask 111)
           (allocation_decay_range 000) (bank_used_for_prediction 11)
           (controlled_allocation_throttler 0001000) (end_allocation_bank 00)
           (end_allocation_bank_mask 000) (end_allocation_bank_valid 0)
           (first_hitter_bank 00) (first_hitter_mask 011) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0001000) (next_meta 0000)
           (randoms$1 11100100000100111101101001111000)
           (randoms$2 01110010010010010011100101010011)
           (randoms$3 01111110011110001100100010011101)
           (randoms$4 11100011100001010000111110111001)
           (randoms$5 10001110110110111111111011111110) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 0) (zzbank-eq2 0) (zznottakens0 01)
           (zznottakens1 01) (zznottakens2 01) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 00)
           (zztakens1 00) (zztakens2 00))))

        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 0) (resolved_direction 0)
             (branch_target 11111111111111111111110001001011)))
           (read_entries
            (((tag 0000001) (counters ((num_takens 00) (num_not_takens 10))))
             ((tag 0000001) (counters ((num_takens 01) (num_not_takens 11))))
             ((tag 0000000) (counters ((num_takens 11) (num_not_takens 00))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 1) (hysteresis 1)))))
         (outputs
          ((next_entries
            (((tag 0000001) (counters ((num_takens 00) (num_not_takens 10))))
             ((tag 0000001) (counters ((num_takens 01) (num_not_takens 11))))
             ((tag 0000000) (counters ((num_takens 11) (num_not_takens 01))))))
           (next_bimodal_entry ((direction 1) (hysteresis 1))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 11) (allocation_bank_mask 111)
           (allocation_decay_range 100) (bank_used_for_prediction 10)
           (controlled_allocation_throttler 0001000) (end_allocation_bank 01)
           (end_allocation_bank_mask 100) (end_allocation_bank_valid 1)
           (first_hitter_bank 10) (first_hitter_mask 110) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0001000) (next_meta 0000)
           (randoms$1 11100100000100111101101001111000)
           (randoms$2 01110010010010010011100101010011)
           (randoms$3 01111110011110001100100010011101)
           (randoms$4 11100011100001010000111110111001)
           (randoms$5 10001110110110111111111011111110) (skipped_allocation_banks 01)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 0) (zzbank-eq2 0) (zznottakens0 01)
           (zznottakens1 01) (zznottakens2 01) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 00)
           (zztakens1 00) (zztakens2 00))))

        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 0) (resolved_direction 1)
             (branch_target 00011101001011011000101111001100)))
           (read_entries
            (((tag 0000000) (counters ((num_takens 01) (num_not_takens 00))))
             ((tag 0000000) (counters ((num_takens 11) (num_not_takens 00))))
             ((tag 0000001) (counters ((num_takens 10) (num_not_takens 10))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 1) (hysteresis 0)))))
         (outputs
          ((next_entries
            (((tag 0000000) (counters ((num_takens 10) (num_not_takens 00))))
             ((tag 0000000) (counters ((num_takens 11) (num_not_takens 00))))
             ((tag 0000001) (counters ((num_takens 10) (num_not_takens 10))))))
           (next_bimodal_entry ((direction 1) (hysteresis 0))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 11) (allocation_bank_mask 111)
           (allocation_decay_range 000) (bank_used_for_prediction 00)
           (controlled_allocation_throttler 0001000) (end_allocation_bank 00)
           (end_allocation_bank_mask 000) (end_allocation_bank_valid 0)
           (first_hitter_bank 00) (first_hitter_mask 011) (gnd 0)
           (hitter_after_prediction_bank 01) (mhc 0)
           (next_controlled_allocation_throttler 0001000) (next_meta 0000)
           (randoms$1 11100100000100111101101001111000)
           (randoms$2 01110010010010010011100101010011)
           (randoms$3 01111110011110001100100010011101)
           (randoms$4 11100011100001010000111110111001)
           (randoms$5 10001110110110111111111011111110) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 0) (zzbank-eq2 0) (zznottakens0 00)
           (zznottakens1 00) (zznottakens2 00) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 01)
           (zztakens1 01) (zztakens2 01))))

        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 1) (resolved_direction 1)
             (branch_target 11111111111111111111110111000011)))
           (read_entries
            (((tag 0000001) (counters ((num_takens 11) (num_not_takens 00))))
             ((tag 0000001) (counters ((num_takens 00) (num_not_takens 11))))
             ((tag 0000001) (counters ((num_takens 11) (num_not_takens 11))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 1) (hysteresis 0)))))
         (outputs
          ((next_entries
            (((tag 0000001) (counters ((num_takens 11) (num_not_takens 00))))
             ((tag 0000001) (counters ((num_takens 00) (num_not_takens 11))))
             ((tag 0000001) (counters ((num_takens 11) (num_not_takens 11))))))
           (next_bimodal_entry ((direction 1) (hysteresis 1))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 10) (allocation_bank_mask 000)
           (allocation_decay_range 000) (bank_used_for_prediction 11)
           (controlled_allocation_throttler 0001000) (end_allocation_bank 11)
           (end_allocation_bank_mask 111) (end_allocation_bank_valid 1)
           (first_hitter_bank 11) (first_hitter_mask 111) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0001000) (next_meta 0000)
           (randoms$1 11100100000100111101101001111000)
           (randoms$2 01110010010010010011100101010011)
           (randoms$3 01111110011110001100100010011101)
           (randoms$4 11100011100001010000111110111001)
           (randoms$5 10001110110110111111111011111110) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 0) (zzbank-eq2 1) (zznottakens0 00)
           (zznottakens1 00) (zznottakens2 00) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 01)
           (zztakens1 01) (zztakens2 01))))

        ((bp 0) ("List.nth hits bp" ()) ("List.length hits" 0)
         (entries
          ((((num_takens 3) (num_not_takens 0)) false)
           (((num_takens 0) (num_not_takens 3)) false)
           (((num_takens 3) (num_not_takens 3)) false))))
        (update_entry (i 0))
        (model
         ((meta 0) (cat 8) (random1 1959669526) (random2 4178382754)
          (random3 2315271720) (random4 204725888) (random5 3515648508)))
        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 0) (resolved_direction 1)
             (branch_target 00000000000000000000010100000010)))
           (read_entries
            (((tag 0000000) (counters ((num_takens 01) (num_not_takens 11))))
             ((tag 0000000) (counters ((num_takens 00) (num_not_takens 00))))
             ((tag 0000000) (counters ((num_takens 01) (num_not_takens 01))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 1) (hysteresis 1)))))
         (outputs
          ((next_entries
            (((tag 0000000) (counters ((num_takens 10) (num_not_takens 11))))
             ((tag 0000000) (counters ((num_takens 01) (num_not_takens 00))))
             ((tag 0000000) (counters ((num_takens 10) (num_not_takens 01))))))
           (next_bimodal_entry ((direction 1) (hysteresis 1))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 11) (allocation_bank_mask 111)
           (allocation_decay_range 000) (bank_used_for_prediction 11)
           (controlled_allocation_throttler 0001000) (end_allocation_bank 00)
           (end_allocation_bank_mask 000) (end_allocation_bank_valid 0)
           (first_hitter_bank 00) (first_hitter_mask 011) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0001000) (next_meta 0000)
           (randoms$1 01110100110011100010111100010110)
           (randoms$2 11111001000011010000111110100010)
           (randoms$3 10001010000000000011111000101000)
           (randoms$4 00001100001100111101111010000000)
           (randoms$5 11010001100011001000100111111100) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 0) (zzbank-eq2 0) (zznottakens0 00)
           (zznottakens1 00) (zznottakens2 00) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 01)
           (zztakens1 01) (zztakens2 01))))

        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 1) (resolved_direction 1)
             (branch_target 00000000000000000000000000000000)))
           (read_entries
            (((tag 0000000) (counters ((num_takens 10) (num_not_takens 01))))
             ((tag 0000001) (counters ((num_takens 11) (num_not_takens 10))))
             ((tag 0000001) (counters ((num_takens 10) (num_not_takens 01))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 1) (hysteresis 1)))))
         (outputs
          ((next_entries
            (((tag 0000000) (counters ((num_takens 11) (num_not_takens 01))))
             ((tag 0000001) (counters ((num_takens 11) (num_not_takens 10))))
             ((tag 0000001) (counters ((num_takens 10) (num_not_takens 01))))))
           (next_bimodal_entry ((direction 1) (hysteresis 1))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 11) (allocation_bank_mask 111)
           (allocation_decay_range 000) (bank_used_for_prediction 11)
           (controlled_allocation_throttler 0001000) (end_allocation_bank 00)
           (end_allocation_bank_mask 000) (end_allocation_bank_valid 0)
           (first_hitter_bank 00) (first_hitter_mask 011) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0001000) (next_meta 0000)
           (randoms$1 01110100110011100010111100010110)
           (randoms$2 11111001000011010000111110100010)
           (randoms$3 10001010000000000011111000101000)
           (randoms$4 00001100001100111101111010000000)
           (randoms$5 11010001100011001000100111111100) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 0) (zzbank-eq2 0) (zznottakens0 00)
           (zznottakens1 00) (zznottakens2 00) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 01)
           (zztakens1 01) (zztakens2 01))))

        ((bp 1) ("List.nth hits bp" ()) ("List.length hits" 1)
         (entries
          ((((num_takens 2) (num_not_takens 1)) true)
           (((num_takens 3) (num_not_takens 2)) false)
           (((num_takens 2) (num_not_takens 1)) false))))
        (update_entry (i 1))
        (model
         ((meta 0) (cat 8) (random1 2495235968) (random2 1205147678)
          (random3 896164234) (random4 2342472529) (random5 1456872709)))
        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 1) (resolved_direction 1)
             (branch_target 00000000000000000000000000110111)))
           (read_entries
            (((tag 0000001) (counters ((num_takens 10) (num_not_takens 00))))
             ((tag 0000000) (counters ((num_takens 11) (num_not_takens 10))))
             ((tag 0000001) (counters ((num_takens 10) (num_not_takens 01))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 0) (hysteresis 0)))))
         (outputs
          ((next_entries
            (((tag 0000001) (counters ((num_takens 10) (num_not_takens 00))))
             ((tag 0000000) (counters ((num_takens 11) (num_not_takens 01))))
             ((tag 0000001) (counters ((num_takens 10) (num_not_takens 01))))))
           (next_bimodal_entry ((direction 1) (hysteresis 0))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 11) (allocation_bank_mask 111)
           (allocation_decay_range 100) (bank_used_for_prediction 11)
           (controlled_allocation_throttler 0001000) (end_allocation_bank 01)
           (end_allocation_bank_mask 100) (end_allocation_bank_valid 1)
           (first_hitter_bank 01) (first_hitter_mask 101) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0001000) (next_meta 0000)
           (randoms$1 10010100101110100100011110000000)
           (randoms$2 01000111110101010001100000011110)
           (randoms$3 00110101011010100110000110001010)
           (randoms$4 10001011100111110100101101010001)
           (randoms$5 01010110110101100001110100000101) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 0) (zzbank-eq2 0) (zznottakens0 00)
           (zznottakens1 00) (zznottakens2 00) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 01)
           (zztakens1 01) (zztakens2 01))))

        ((bp 1) ("List.nth hits bp" ()) ("List.length hits" 1)
         (entries
          ((((num_takens 2) (num_not_takens 0)) false)
           (((num_takens 3) (num_not_takens 2)) true)
           (((num_takens 2) (num_not_takens 1)) false))))
        (update_entry (i 1))
        allocate
        (i 1)
        ((!i 1) ("diff (List.hd_exn s)" 1))
        ((decay_bank 0) (!mhc 1))
        (model
         ((meta 0) (cat 8) (random1 1427053829) (random2 2114184405)
          (random3 1379449287) (random4 1012801355) (random5 1000865918)))
        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 1) (resolved_direction 0)
             (branch_target 00110100111110010000011110101110)))
           (read_entries
            (((tag 0000001) (counters ((num_takens 11) (num_not_takens 10))))
             ((tag 0000001) (counters ((num_takens 10) (num_not_takens 11))))
             ((tag 0000001) (counters ((num_takens 11) (num_not_takens 01))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 0) (hysteresis 1)))))
         (outputs
          ((next_entries
            (((tag 0000001) (counters ((num_takens 11) (num_not_takens 10))))
             ((tag 0000001) (counters ((num_takens 10) (num_not_takens 11))))
             ((tag 0000001) (counters ((num_takens 11) (num_not_takens 01))))))
           (next_bimodal_entry ((direction 0) (hysteresis 1))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 10) (allocation_bank_mask 000)
           (allocation_decay_range 000) (bank_used_for_prediction 11)
           (controlled_allocation_throttler 0001000) (end_allocation_bank 11)
           (end_allocation_bank_mask 111) (end_allocation_bank_valid 1)
           (first_hitter_bank 11) (first_hitter_mask 111) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0001000) (next_meta 0000)
           (randoms$1 01010101000011110001110100000101)
           (randoms$2 01111110000000111110010011010101)
           (randoms$3 01010010001110001011100111000111)
           (randoms$4 00111100010111100001111101001011)
           (randoms$5 00111011101010000000000001111110) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 0) (zzbank-eq2 1) (zznottakens0 01)
           (zznottakens1 01) (zznottakens2 01) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 00)
           (zztakens1 00) (zztakens2 00))))

        ((bp 0) ("List.nth hits bp" ()) ("List.length hits" 0)
         (entries
          ((((num_takens 3) (num_not_takens 2)) false)
           (((num_takens 2) (num_not_takens 3)) false)
           (((num_takens 3) (num_not_takens 1)) false))))
        (update_entry (i 0))
        (model
         ((meta 0) (cat 8) (random1 1666395154) (random2 1370617529)
          (random3 3842970535) (random4 156701328) (random5 1331456013)))
        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 1) (resolved_direction 0)
             (branch_target 01110001000010101111000101111010)))
           (read_entries
            (((tag 0000000) (counters ((num_takens 10) (num_not_takens 11))))
             ((tag 0000000) (counters ((num_takens 11) (num_not_takens 00))))
             ((tag 0000001) (counters ((num_takens 01) (num_not_takens 00))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 1) (hysteresis 1)))))
         (outputs
          ((next_entries
            (((tag 0000000) (counters ((num_takens 01) (num_not_takens 11))))
             ((tag 0000000) (counters ((num_takens 11) (num_not_takens 01))))
             ((tag 0000001) (counters ((num_takens 01) (num_not_takens 00))))))
           (next_bimodal_entry ((direction 1) (hysteresis 1))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 11) (allocation_bank_mask 111)
           (allocation_decay_range 000) (bank_used_for_prediction 01)
           (controlled_allocation_throttler 0001000) (end_allocation_bank 00)
           (end_allocation_bank_mask 000) (end_allocation_bank_valid 0)
           (first_hitter_bank 00) (first_hitter_mask 011) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0001000) (next_meta 0000)
           (randoms$1 01100011010100110010110000010010)
           (randoms$2 01010001101100011111011010111001)
           (randoms$3 11100101000011110001001110100111)
           (randoms$4 00001001010101110001001010010000)
           (randoms$5 01001111010111000110100000001101) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 0) (zzbank-eq2 0) (zznottakens0 01)
           (zznottakens1 01) (zznottakens2 01) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 00)
           (zztakens1 00) (zztakens2 00))))

        ((bp 1) ("List.nth hits bp" (1)) ("List.length hits" 2)
         (entries
          ((((num_takens 2) (num_not_takens 3)) true)
           (((num_takens 3) (num_not_takens 0)) true)
           (((num_takens 1) (num_not_takens 0)) false))))
        (update_entry (i 1))
        allocate
        (i 0)
        ((!i 0) ("diff (List.hd_exn s)" 1))
        (model
         ((meta 0) (cat 8) (random1 3707535418) (random2 2210054218)
          (random3 3892704090) (random4 2343142738) (random5 2313719107)))
        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 0) (resolved_direction 0)
             (branch_target 11111111111111111000010110101000)))
           (read_entries
            (((tag 0000001) (counters ((num_takens 11) (num_not_takens 00))))
             ((tag 0000001) (counters ((num_takens 00) (num_not_takens 10))))
             ((tag 0000000) (counters ((num_takens 01) (num_not_takens 00))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 1) (hysteresis 0)))))
         (outputs
          ((next_entries
            (((tag 0000001) (counters ((num_takens 11) (num_not_takens 00))))
             ((tag 0000001) (counters ((num_takens 00) (num_not_takens 10))))
             ((tag 0000000) (counters ((num_takens 01) (num_not_takens 01))))))
           (next_bimodal_entry ((direction 0) (hysteresis 0))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 11) (allocation_bank_mask 111)
           (allocation_decay_range 110) (bank_used_for_prediction 10)
           (controlled_allocation_throttler 0001000) (end_allocation_bank 10)
           (end_allocation_bank_mask 110) (end_allocation_bank_valid 1)
           (first_hitter_bank 10) (first_hitter_mask 110) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0001000) (next_meta 0000)
           (randoms$1 11011100111111001000000000111010)
           (randoms$2 10000011101110101100000001001010)
           (randoms$3 11101000000001011111001101011010)
           (randoms$4 10001011101010011000010101010010)
           (randoms$5 10001001111010001000110101000011) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 0) (zzbank-eq2 0) (zznottakens0 01)
           (zznottakens1 01) (zznottakens2 01) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 00)
           (zztakens1 00) (zztakens2 00))))

        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 1) (resolved_direction 1)
             (branch_target 00000000011011010111110101110100)))
           (read_entries
            (((tag 0000001) (counters ((num_takens 11) (num_not_takens 10))))
             ((tag 0000001) (counters ((num_takens 11) (num_not_takens 00))))
             ((tag 0000000) (counters ((num_takens 01) (num_not_takens 11))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 1) (hysteresis 0)))))
         (outputs
          ((next_entries
            (((tag 0000001) (counters ((num_takens 11) (num_not_takens 10))))
             ((tag 0000001) (counters ((num_takens 11) (num_not_takens 00))))
             ((tag 0000000) (counters ((num_takens 10) (num_not_takens 11))))))
           (next_bimodal_entry ((direction 1) (hysteresis 1))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 00) (allocation_bank_mask 011)
           (allocation_decay_range 010) (bank_used_for_prediction 11)
           (controlled_allocation_throttler 0001000) (end_allocation_bank 10)
           (end_allocation_bank_mask 110) (end_allocation_bank_valid 1)
           (first_hitter_bank 10) (first_hitter_mask 110) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0001000) (next_meta 0000)
           (randoms$1 11011100111111001000000000111010)
           (randoms$2 10000011101110101100000001001010)
           (randoms$3 11101000000001011111001101011010)
           (randoms$4 10001011101010011000010101010010)
           (randoms$5 10001001111010001000110101000011) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 1) (zzbank-eq1 0) (zzbank-eq2 0) (zznottakens0 00)
           (zznottakens1 00) (zznottakens2 00) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 01)
           (zztakens1 01) (zztakens2 01))))

        ((bp 1) ("List.nth hits bp" ()) ("List.length hits" 1)
         (entries
          ((((num_takens 3) (num_not_takens 2)) false)
           (((num_takens 3) (num_not_takens 0)) false)
           (((num_takens 1) (num_not_takens 3)) true))))
        (update_entry (i 1))
        (model
         ((meta 0) (cat 8) (random1 3548851879) (random2 2915983347)
          (random3 2612893133) (random4 3681583795) (random5 2421460227)))
        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 1) (resolved_direction 1)
             (branch_target 00000000110111110101011101001101)))
           (read_entries
            (((tag 0000001) (counters ((num_takens 00) (num_not_takens 10))))
             ((tag 0000001) (counters ((num_takens 10) (num_not_takens 01))))
             ((tag 0000000) (counters ((num_takens 01) (num_not_takens 10))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 0) (hysteresis 0)))))
         (outputs
          ((next_entries
            (((tag 0000001) (counters ((num_takens 00) (num_not_takens 10))))
             ((tag 0000000) (counters ((num_takens 01) (num_not_takens 00))))
             ((tag 0000000) (counters ((num_takens 10) (num_not_takens 10))))))
           (next_bimodal_entry ((direction 1) (hysteresis 0))) (meta 0000)))
         (internals
          ((allocate 1) (allocation_bank 01) (allocation_bank_mask 001)
           (allocation_decay_range 000) (bank_used_for_prediction 11)
           (controlled_allocation_throttler 0001000) (end_allocation_bank 10)
           (end_allocation_bank_mask 110) (end_allocation_bank_valid 1)
           (first_hitter_bank 10) (first_hitter_mask 110) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0001010) (next_meta 0000)
           (randoms$1 11010011100001110010111010100111)
           (randoms$2 10101101110011100101111111110011)
           (randoms$3 10011011101111011001010111001101)
           (randoms$4 11011011011100001000001010110011)
           (randoms$5 10010000010101001000110100000011) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 1) (zzbank-eq2 0) (zznottakens0 00)
           (zznottakens1 00) (zznottakens2 00) (zzreallyallocate 1)
           (zzreallyallocate_0 1) (zzreallyallocate_1 1) (zztakens0 01)
           (zztakens1 01) (zztakens2 01))))

        ((bp 1) ("List.nth hits bp" ()) ("List.length hits" 1)
         (entries
          ((((num_takens 0) (num_not_takens 2)) false)
           (((num_takens 2) (num_not_takens 1)) false)
           (((num_takens 1) (num_not_takens 2)) true))))
        (update_entry (i 1))
        allocate
        (i 2)
        ((!i 2) ("diff (List.hd_exn s)" 1))
        (allocation_bank 1)
        (model
         ((meta 0) (cat 10) (random1 4230571086) (random2 2687603083)
          (random3 159385775) (random4 2952160032) (random5 1063867097)))
        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 1) (resolved_direction 1)
             (branch_target 00100010010110000100010011100010)))
           (read_entries
            (((tag 0000001) (counters ((num_takens 10) (num_not_takens 00))))
             ((tag 0000000) (counters ((num_takens 00) (num_not_takens 11))))
             ((tag 0000001) (counters ((num_takens 00) (num_not_takens 01))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 0) (hysteresis 1)))))
         (outputs
          ((next_entries
            (((tag 0000001) (counters ((num_takens 10) (num_not_takens 00))))
             ((tag 0000000) (counters ((num_takens 01) (num_not_takens 11))))
             ((tag 0000001) (counters ((num_takens 00) (num_not_takens 01))))))
           (next_bimodal_entry ((direction 0) (hysteresis 1))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 11) (allocation_bank_mask 111)
           (allocation_decay_range 100) (bank_used_for_prediction 01)
           (controlled_allocation_throttler 0001010) (end_allocation_bank 01)
           (end_allocation_bank_mask 100) (end_allocation_bank_valid 1)
           (first_hitter_bank 01) (first_hitter_mask 101) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0001010) (next_meta 0000)
           (randoms$1 11111100001010010110010001001110)
           (randoms$2 10100000001100011001000110001011)
           (randoms$3 00001001100000000000100010101111)
           (randoms$4 10101111111101100110001100100000)
           (randoms$5 00111111011010010101001011011001) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 0) (zzbank-eq2 0) (zznottakens0 00)
           (zznottakens1 00) (zznottakens2 00) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 01)
           (zztakens1 01) (zztakens2 01))))

        ((bp 0) ("List.nth hits bp" (1)) ("List.length hits" 1)
         (entries
          ((((num_takens 2) (num_not_takens 0)) false)
           (((num_takens 0) (num_not_takens 3)) true)
           (((num_takens 0) (num_not_takens 1)) false))))
        (update_entry (i 0))
        allocate
        (i 1)
        ((!i 1) ("diff (List.hd_exn s)" 3))
        ((decay_bank 0) (!mhc 1))
        (model
         ((meta 0) (cat 10) (random1 3300478942) (random2 3524766187)
          (random3 438857797) (random4 299372489) (random5 3011342400)))
        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 1) (resolved_direction 0)
             (branch_target 00010001010001011000101100100111)))
           (read_entries
            (((tag 0000000) (counters ((num_takens 11) (num_not_takens 11))))
             ((tag 0000001) (counters ((num_takens 10) (num_not_takens 01))))
             ((tag 0000001) (counters ((num_takens 00) (num_not_takens 10))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 1) (hysteresis 1)))))
         (outputs
          ((next_entries
            (((tag 0000000) (counters ((num_takens 10) (num_not_takens 11))))
             ((tag 0000001) (counters ((num_takens 10) (num_not_takens 01))))
             ((tag 0000001) (counters ((num_takens 00) (num_not_takens 10))))))
           (next_bimodal_entry ((direction 1) (hysteresis 0))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 11) (allocation_bank_mask 111)
           (allocation_decay_range 000) (bank_used_for_prediction 11)
           (controlled_allocation_throttler 0001010) (end_allocation_bank 00)
           (end_allocation_bank_mask 000) (end_allocation_bank_valid 0)
           (first_hitter_bank 00) (first_hitter_mask 011) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0001010) (next_meta 0000)
           (randoms$1 11000100101110010100111111011110)
           (randoms$2 11010010000101111010100111101011)
           (randoms$3 00011010001010000111000001000101)
           (randoms$4 00010001110110000000111111001001)
           (randoms$5 10110011011111010111000001000000) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 0) (zzbank-eq2 0) (zznottakens0 01)
           (zznottakens1 01) (zznottakens2 01) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 00)
           (zztakens1 00) (zztakens2 00))))

        ((bp 1) ("List.nth hits bp" ()) ("List.length hits" 1)
         (entries
          ((((num_takens 3) (num_not_takens 3)) true)
           (((num_takens 2) (num_not_takens 1)) false)
           (((num_takens 0) (num_not_takens 2)) false))))
        (update_entry (i 1))
        allocate
        (i 0)
        ((!i 0) ("diff (List.hd_exn s)" 0))
        (model
         ((meta 0) (cat 10) (random1 1159583391) (random2 3262223806)
          (random3 2419822837) (random4 335928569) (random5 3017449914)))
        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 1) (resolved_direction 1)
             (branch_target 11111101000000111111100010010101)))
           (read_entries
            (((tag 0000001) (counters ((num_takens 11) (num_not_takens 10))))
             ((tag 0000000) (counters ((num_takens 11) (num_not_takens 01))))
             ((tag 0000001) (counters ((num_takens 00) (num_not_takens 11))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 0) (hysteresis 1)))))
         (outputs
          ((next_entries
            (((tag 0000000) (counters ((num_takens 01) (num_not_takens 00))))
             ((tag 0000000) (counters ((num_takens 11) (num_not_takens 00))))
             ((tag 0000001) (counters ((num_takens 00) (num_not_takens 11))))))
           (next_bimodal_entry ((direction 0) (hysteresis 0))) (meta 0000)))
         (internals
          ((allocate 1) (allocation_bank 00) (allocation_bank_mask 011)
           (allocation_decay_range 000) (bank_used_for_prediction 11)
           (controlled_allocation_throttler 0001010) (end_allocation_bank 01)
           (end_allocation_bank_mask 100) (end_allocation_bank_valid 1)
           (first_hitter_bank 01) (first_hitter_mask 101) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0001100) (next_meta 0000)
           (randoms$1 01000101000111011101011010011111)
           (randoms$2 11000010011100011001010110111110)
           (randoms$3 10010000001110111001000011110101)
           (randoms$4 00010100000001011101110011111001)
           (randoms$5 10110011110110101010000110111010) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 1) (zzbank-eq1 0) (zzbank-eq2 0) (zznottakens0 00)
           (zznottakens1 00) (zznottakens2 00) (zzreallyallocate 1)
           (zzreallyallocate_0 1) (zzreallyallocate_1 1) (zztakens0 01)
           (zztakens1 01) (zztakens2 01))))

        ((bp 1) ("List.nth hits bp" ()) ("List.length hits" 1)
         (entries
          ((((num_takens 3) (num_not_takens 2)) false)
           (((num_takens 3) (num_not_takens 1)) true)
           (((num_takens 0) (num_not_takens 3)) false))))
        (update_entry (i 1))
        allocate
        (i 1)
        ((!i 1) ("diff (List.hd_exn s)" 2))
        (allocation_bank 0)
        (model
         ((meta 0) (cat 12) (random1 101148280) (random2 3892549245)
          (random3 2794290439) (random4 1557122484) (random5 442489804)))
        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 0) (resolved_direction 0)
             (branch_target 11111111111111111111111111111111)))
           (read_entries
            (((tag 0000001) (counters ((num_takens 10) (num_not_takens 00))))
             ((tag 0000001) (counters ((num_takens 01) (num_not_takens 00))))
             ((tag 0000000) (counters ((num_takens 11) (num_not_takens 01))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 0) (hysteresis 0)))))
         (outputs
          ((next_entries
            (((tag 0000001) (counters ((num_takens 10) (num_not_takens 00))))
             ((tag 0000001) (counters ((num_takens 01) (num_not_takens 00))))
             ((tag 0000000) (counters ((num_takens 11) (num_not_takens 10))))))
           (next_bimodal_entry ((direction 0) (hysteresis 1))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 01) (allocation_bank_mask 001)
           (allocation_decay_range 000) (bank_used_for_prediction 11)
           (controlled_allocation_throttler 0001100) (end_allocation_bank 10)
           (end_allocation_bank_mask 110) (end_allocation_bank_valid 1)
           (first_hitter_bank 10) (first_hitter_mask 110) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0001100) (next_meta 0000)
           (randoms$1 00000110000001110110011001111000)
           (randoms$2 11101000000000111001011001111101)
           (randoms$3 10100110100011010111110100000111)
           (randoms$4 01011100110011111100110110110100)
           (randoms$5 00011010010111111101101111001100) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 1) (zzbank-eq2 0) (zznottakens0 01)
           (zznottakens1 01) (zznottakens2 01) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 00)
           (zztakens1 00) (zztakens2 00))))

        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 0) (resolved_direction 0)
             (branch_target 10100001111101011010110100100001)))
           (read_entries
            (((tag 0000001) (counters ((num_takens 00) (num_not_takens 00))))
             ((tag 0000001) (counters ((num_takens 01) (num_not_takens 11))))
             ((tag 0000001) (counters ((num_takens 10) (num_not_takens 11))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 1) (hysteresis 0)))))
         (outputs
          ((next_entries
            (((tag 0000001) (counters ((num_takens 00) (num_not_takens 00))))
             ((tag 0000001) (counters ((num_takens 01) (num_not_takens 11))))
             ((tag 0000000) (counters ((num_takens 00) (num_not_takens 01))))))
           (next_bimodal_entry ((direction 0) (hysteresis 0))) (meta 0000)))
         (internals
          ((allocate 1) (allocation_bank 10) (allocation_bank_mask 000)
           (allocation_decay_range 000) (bank_used_for_prediction 11)
           (controlled_allocation_throttler 0001100) (end_allocation_bank 11)
           (end_allocation_bank_mask 111) (end_allocation_bank_valid 1)
           (first_hitter_bank 11) (first_hitter_mask 111) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0001110) (next_meta 0000)
           (randoms$1 00000110000001110110011001111000)
           (randoms$2 11101000000000111001011001111101)
           (randoms$3 10100110100011010111110100000111)
           (randoms$4 01011100110011111100110110110100)
           (randoms$5 00011010010111111101101111001100) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 0) (zzbank-eq2 1) (zznottakens0 01)
           (zznottakens1 01) (zznottakens2 01) (zzreallyallocate 1)
           (zzreallyallocate_0 1) (zzreallyallocate_1 1) (zztakens0 00)
           (zztakens1 00) (zztakens2 00))))

        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 1) (resolved_direction 1)
             (branch_target 10000111011111010100001110000000)))
           (read_entries
            (((tag 0000000) (counters ((num_takens 11) (num_not_takens 01))))
             ((tag 0000000) (counters ((num_takens 10) (num_not_takens 01))))
             ((tag 0000000) (counters ((num_takens 01) (num_not_takens 01))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 1) (hysteresis 1)))))
         (outputs
          ((next_entries
            (((tag 0000000) (counters ((num_takens 11) (num_not_takens 00))))
             ((tag 0000000) (counters ((num_takens 11) (num_not_takens 01))))
             ((tag 0000000) (counters ((num_takens 10) (num_not_takens 01))))))
           (next_bimodal_entry ((direction 1) (hysteresis 1))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 11) (allocation_bank_mask 111)
           (allocation_decay_range 000) (bank_used_for_prediction 11)
           (controlled_allocation_throttler 0001100) (end_allocation_bank 00)
           (end_allocation_bank_mask 000) (end_allocation_bank_valid 0)
           (first_hitter_bank 00) (first_hitter_mask 011) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0001100) (next_meta 0000)
           (randoms$1 00000110000001110110011001111000)
           (randoms$2 11101000000000111001011001111101)
           (randoms$3 10100110100011010111110100000111)
           (randoms$4 01011100110011111100110110110100)
           (randoms$5 00011010010111111101101111001100) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 0) (zzbank-eq2 0) (zznottakens0 00)
           (zznottakens1 00) (zznottakens2 00) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 01)
           (zztakens1 01) (zztakens2 01))))

        ((bp 3) ("List.nth hits bp" ()) ("List.length hits" 3)
         (entries
          ((((num_takens 3) (num_not_takens 1)) true)
           (((num_takens 2) (num_not_takens 1)) true)
           (((num_takens 1) (num_not_takens 1)) true))))
        (update_entry (i 3))
        (model
         ((meta 0) (cat 12) (random1 3016388764) (random2 3275954299)
          (random3 748596145) (random4 173735432) (random5 3320004799)))
        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 0) (resolved_direction 0)
             (branch_target 00100110111111100110111001010111)))
           (read_entries
            (((tag 0000001) (counters ((num_takens 11) (num_not_takens 11))))
             ((tag 0000001) (counters ((num_takens 00) (num_not_takens 11))))
             ((tag 0000001) (counters ((num_takens 01) (num_not_takens 01))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 1) (hysteresis 1)))))
         (outputs
          ((next_entries
            (((tag 0000001) (counters ((num_takens 11) (num_not_takens 11))))
             ((tag 0000001) (counters ((num_takens 00) (num_not_takens 11))))
             ((tag 0000000) (counters ((num_takens 00) (num_not_takens 01))))))
           (next_bimodal_entry ((direction 1) (hysteresis 0))) (meta 0000)))
         (internals
          ((allocate 1) (allocation_bank 10) (allocation_bank_mask 000)
           (allocation_decay_range 000) (bank_used_for_prediction 11)
           (controlled_allocation_throttler 0001100) (end_allocation_bank 11)
           (end_allocation_bank_mask 111) (end_allocation_bank_valid 1)
           (first_hitter_bank 11) (first_hitter_mask 111) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0001110) (next_meta 0000)
           (randoms$1 10110011110010100111000010011100)
           (randoms$2 11000011010000110001100001111011)
           (randoms$3 00101100100111101010101110110001)
           (randoms$4 00001010010110101111111000001000)
           (randoms$5 11000101111000110100000010111111) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 0) (zzbank-eq2 1) (zznottakens0 01)
           (zznottakens1 01) (zznottakens2 01) (zzreallyallocate 1)
           (zzreallyallocate_0 1) (zzreallyallocate_1 1) (zztakens0 00)
           (zztakens1 00) (zztakens2 00))))

        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 0) (resolved_direction 1)
             (branch_target 01111110010000011001000110000110)))
           (read_entries
            (((tag 0000001) (counters ((num_takens 00) (num_not_takens 11))))
             ((tag 0000001) (counters ((num_takens 10) (num_not_takens 10))))
             ((tag 0000000) (counters ((num_takens 00) (num_not_takens 00))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 1) (hysteresis 0)))))
         (outputs
          ((next_entries
            (((tag 0000001) (counters ((num_takens 00) (num_not_takens 11))))
             ((tag 0000001) (counters ((num_takens 10) (num_not_takens 10))))
             ((tag 0000000) (counters ((num_takens 01) (num_not_takens 00))))))
           (next_bimodal_entry ((direction 1) (hysteresis 1))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 01) (allocation_bank_mask 001)
           (allocation_decay_range 000) (bank_used_for_prediction 11)
           (controlled_allocation_throttler 0001100) (end_allocation_bank 10)
           (end_allocation_bank_mask 110) (end_allocation_bank_valid 1)
           (first_hitter_bank 10) (first_hitter_mask 110) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0001100) (next_meta 0000)
           (randoms$1 10110011110010100111000010011100)
           (randoms$2 11000011010000110001100001111011)
           (randoms$3 00101100100111101010101110110001)
           (randoms$4 00001010010110101111111000001000)
           (randoms$5 11000101111000110100000010111111) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 0) (zzbank-eq1 1) (zzbank-eq2 0) (zznottakens0 00)
           (zznottakens1 00) (zznottakens2 00) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 01)
           (zztakens1 01) (zztakens2 01))))

        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 1) (resolved_direction 1)
             (branch_target 00000000000110111111001011010111)))
           (read_entries
            (((tag 0000001) (counters ((num_takens 00) (num_not_takens 01))))
             ((tag 0000000) (counters ((num_takens 11) (num_not_takens 11))))
             ((tag 0000001) (counters ((num_takens 11) (num_not_takens 00))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 1) (hysteresis 0)))))
         (outputs
          ((next_entries
            (((tag 0000001) (counters ((num_takens 00) (num_not_takens 01))))
             ((tag 0000000) (counters ((num_takens 11) (num_not_takens 10))))
             ((tag 0000001) (counters ((num_takens 11) (num_not_takens 00))))))
           (next_bimodal_entry ((direction 1) (hysteresis 1))) (meta 0000)))
         (internals
          ((allocate 0) (allocation_bank 00) (allocation_bank_mask 011)
           (allocation_decay_range 000) (bank_used_for_prediction 11)
           (controlled_allocation_throttler 0001100) (end_allocation_bank 01)
           (end_allocation_bank_mask 100) (end_allocation_bank_valid 1)
           (first_hitter_bank 01) (first_hitter_mask 101) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0001100) (next_meta 0000)
           (randoms$1 10110011110010100111000010011100)
           (randoms$2 11000011010000110001100001111011)
           (randoms$3 00101100100111101010101110110001)
           (randoms$4 00001010010110101111111000001000)
           (randoms$5 11000101111000110100000010111111) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 1) (zzbank-eq1 0) (zzbank-eq2 0) (zznottakens0 00)
           (zznottakens1 00) (zznottakens2 00) (zzreallyallocate 0)
           (zzreallyallocate_0 0) (zzreallyallocate_1 0) (zztakens0 01)
           (zztakens1 01) (zztakens2 01))))

        ((bp 1) ("List.nth hits bp" ()) ("List.length hits" 1)
         (entries
          ((((num_takens 0) (num_not_takens 1)) false)
           (((num_takens 3) (num_not_takens 3)) true)
           (((num_takens 3) (num_not_takens 0)) false))))
        (update_entry (i 1))
        (model
         ((meta 0) (cat 12) (random1 1189625968) (random2 2848531453)
          (random3 3304464357) (random4 3873524837) (random5 325730021)))
        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 0) (resolved_direction 1)
             (branch_target 10100000100100011100010011011011)))
           (read_entries
            (((tag 0000001) (counters ((num_takens 01) (num_not_takens 10))))
             ((tag 0000000) (counters ((num_takens 10) (num_not_takens 10))))
             ((tag 0000000) (counters ((num_takens 00) (num_not_takens 11))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 1) (hysteresis 0)))))
         (outputs
          ((next_entries
            (((tag 0000000) (counters ((num_takens 01) (num_not_takens 00))))
             ((tag 0000000) (counters ((num_takens 11) (num_not_takens 10))))
             ((tag 0000000) (counters ((num_takens 01) (num_not_takens 11))))))
           (next_bimodal_entry ((direction 1) (hysteresis 0))) (meta 0000)))
         (internals
          ((allocate 1) (allocation_bank 00) (allocation_bank_mask 011)
           (allocation_decay_range 000) (bank_used_for_prediction 10)
           (controlled_allocation_throttler 0001100) (end_allocation_bank 01)
           (end_allocation_bank_mask 100) (end_allocation_bank_valid 1)
           (first_hitter_bank 01) (first_hitter_mask 101) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0001110) (next_meta 0000)
           (randoms$1 01000110111010000100000001110000)
           (randoms$2 10101001110010010010001111111101)
           (randoms$3 11000100111101100001111111100101)
           (randoms$4 11100110111000010100110001100101)
           (randoms$5 00010011011010100011111011100101) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 1) (zzbank-eq1 0) (zzbank-eq2 0) (zznottakens0 00)
           (zznottakens1 00) (zznottakens2 00) (zzreallyallocate 1)
           (zzreallyallocate_0 1) (zzreallyallocate_1 1) (zztakens0 01)
           (zztakens1 01) (zztakens2 01))))

        ((inputs
          ((clock 0) (clear 0)
           (update
            ((valid 1) (resolved_direction 0)
             (branch_target 00000000000000000000000100011100)))
           (read_entries
            (((tag 0000001) (counters ((num_takens 11) (num_not_takens 01))))
             ((tag 0000000) (counters ((num_takens 01) (num_not_takens 11))))
             ((tag 0000001) (counters ((num_takens 01) (num_not_takens 10))))))
           (tags (0000000 0000000 0000000))
           (bimodal_entry ((direction 1) (hysteresis 1)))))
         (outputs
          ((next_entries
            (((tag 0000001) (counters ((num_takens 11) (num_not_takens 01))))
             ((tag 0000000) (counters ((num_takens 00) (num_not_takens 11))))
             ((tag 0000001) (counters ((num_takens 01) (num_not_takens 10))))))
           (next_bimodal_entry ((direction 1) (hysteresis 0))) (meta 0000)))
         (internals
          ((allocate 1) (allocation_bank 00) (allocation_bank_mask 011)
           (allocation_decay_range 000) (bank_used_for_prediction 11)
           (controlled_allocation_throttler 0001100) (end_allocation_bank 01)
           (end_allocation_bank_mask 100) (end_allocation_bank_valid 1)
           (first_hitter_bank 01) (first_hitter_mask 101) (gnd 0)
           (hitter_after_prediction_bank 11) (mhc 0)
           (next_controlled_allocation_throttler 0001110) (next_meta 0000)
           (randoms$1 01000110111010000100000001110000)
           (randoms$2 10101001110010010010001111111101)
           (randoms$3 11000100111101100001111111100101)
           (randoms$4 11100110111000010100110001100101)
           (randoms$5 00010011011010100011111011100101) (skipped_allocation_banks 00)
           (vdd 1) (zzbank-eq0 1) (zzbank-eq1 0) (zzbank-eq2 0) (zznottakens0 01)
           (zznottakens1 01) (zznottakens2 01) (zzreallyallocate 1)
           (zzreallyallocate_0 1) (zzreallyallocate_1 1) (zztakens0 00)
           (zztakens1 00) (zztakens2 00))))

        ((bp 1) ("List.nth hits bp" ()) ("List.length hits" 1)
         (entries
          ((((num_takens 3) (num_not_takens 1)) false)
           (((num_takens 1) (num_not_takens 3)) true)
           (((num_takens 1) (num_not_takens 2)) false))))
        (update_entry (i 1))
        allocate
        (i 1)
        ((!i 1) ("diff (List.hd_exn s)" 2))
        (allocation_bank 0)
        (model
         ((meta 0) (cat 14) (random1 2452260707) (random2 997089766)
          (random3 3864866912) (random4 584905139) (random5 3261080446))) |}]
      ;;

      let%expect_test "Human" =
        sim ~debug:false ~f:(fun print_state sim ->
          let open Bits in
          let inputs = Cyclesim.inputs sim in
          let next () =
            Cyclesim.cycle sim;
            print_state ();
            ()
          in
          List.iter inputs.tags ~f:(fun tag -> tag := one Params.tag_width);
          inputs.update.valid := vdd;
          next ();
          List.iter inputs.tags ~f:(fun tag -> tag := zero Params.tag_width);
          next ();
          ());
        [%expect
          {|
          ((inputs
            ((clock 0) (clear 0)
             (update
              ((valid 1) (resolved_direction 0)
               (branch_target 00000000000000000000000000000000)))
             (read_entries
              (((tag 0000000) (counters ((num_takens 00) (num_not_takens 00))))
               ((tag 0000000) (counters ((num_takens 00) (num_not_takens 00))))
               ((tag 0000000) (counters ((num_takens 00) (num_not_takens 00))))))
             (tags (0000001 0000001 0000001))
             (bimodal_entry ((direction 0) (hysteresis 0)))))
           (outputs
            ((next_entries
              (((tag 0000000) (counters ((num_takens 00) (num_not_takens 00))))
               ((tag 0000000) (counters ((num_takens 00) (num_not_takens 00))))
               ((tag 0000000) (counters ((num_takens 00) (num_not_takens 00))))))
             (next_bimodal_entry ((direction 0) (hysteresis 1))) (meta 0000)))
           (internals ()))

          ((inputs
            ((clock 0) (clear 0)
             (update
              ((valid 1) (resolved_direction 0)
               (branch_target 00000000000000000000000000000000)))
             (read_entries
              (((tag 0000000) (counters ((num_takens 00) (num_not_takens 00))))
               ((tag 0000000) (counters ((num_takens 00) (num_not_takens 00))))
               ((tag 0000000) (counters ((num_takens 00) (num_not_takens 00))))))
             (tags (0000000 0000000 0000000))
             (bimodal_entry ((direction 0) (hysteresis 0)))))
           (outputs
            ((next_entries
              (((tag 0000000) (counters ((num_takens 00) (num_not_takens 01))))
               ((tag 0000000) (counters ((num_takens 00) (num_not_takens 01))))
               ((tag 0000000) (counters ((num_takens 00) (num_not_takens 01))))))
             (next_bimodal_entry ((direction 0) (hysteresis 1))) (meta 0000)))
           (internals ())) |}]
      ;;
    end
  end
end

module Branch_direction_predictor = struct
  include BayesianTage

  let hierarchical =
    hierarchical
      ~name:"branch_direction_predictor"
      ~params:
        { num_banks = 26
        ; num_entries_per_bank = Int.shift_left 1 7
        ; counter_width = 3
        ; tag_width = 12
        ; controlled_allocation_throttler_max = Int.shift_left 1 16 - 1
        ; max_banks_skipped_on_allocation = 5
        ; meta_width = 5
        ; num_bimodal_direction_entries = Int.shift_left 1 12
        ; num_bimodal_hysteresis_entries = Int.shift_left 1 10
        ; bimodal_hysteresis_width = 2
        ; smallest_branch_history_length = 4
        ; largest_branch_history_length = 700
        ; instruction_window_size = 6
        ; jump_history_length = 30
        ; jump_history_entry_width = 6
        }
  ;;
end
