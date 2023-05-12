open! Core
open Hardcaml

module Branch_target_buffer = struct
  module Entry = struct
    type 'a t =
      { target_pc : 'a [@bits Parameters.word_width]
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
    let hash ~bits address =
      let random_prime =
        of_decimal_string
          ~width:1024
          "155932701214303184326864131316095681640290247695109515520724340505777567578309211013555223897242597556615485113571634396826737509550588918899636445222600177152071201620397262999574183535311513714762158246551801564970296048152341668550491010639120717043516543640455187677926056798474846669042658878200255193621"
      in
      let useful_address = msbs address in
      let bits = Int.min bits (width useful_address) in
      useful_address ^: sel_bottom random_prime (width useful_address)
      |> split_msb ~exact:false ~part_width:bits
      |> List.map ~f:(Fn.flip uresize bits)
      |> reduce ~f:( ^: )
    in
    let size = 1024 in
    hierarchical
      ~name:"branch_target_buffer"
      ~size
      ~address_to_index:(hash ~bits:(address_bits_for size))
      ~address_to_tag:(hash ~bits:13)
  ;;
end

module Return_address_stack = struct
  module Overflowable_stack = struct
    module Make (M : Interface.S) = struct
      module I = struct
        type 'a t =
          { clock : 'a
          ; clear : 'a
          ; push : 'a
          ; pop : 'a
          ; write_data : 'a M.t [@rtlprefix "wr$"]
          }
        [@@deriving sexp_of, hardcaml]
      end

      module O = struct
        type 'a t = { data : 'a M.t [@rtlprefix "rd$"] } [@@deriving sexp_of, hardcaml]
      end

      let create ~size scope { I.clock; clear; push; pop; write_data } =
        assert (size % 2 = 0);
        let open Signal in
        let ( -- ) = Scope.naming scope in
        let entries =
          reg_fb
            ~enable:(push ^: pop)
            ~width:(address_bits_for size)
            ~f:(fun entries -> mux2 push (entries +:. 1) (entries -:. 1))
            (Reg_spec.create ~clock ~clear ())
          -- "entries"
        in
        match
          Ram.create
            ~name:(Scope.name scope "mem")
            ~collision_mode:Read_before_write
            ~size
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
          { O.data =
              M.Of_signal.mux2
                (push |> reg (Reg_spec.create ~clock ()))
                (M.Of_signal.reg ~enable:push (Reg_spec.create ~clock ()) write_data)
                (M.Of_signal.unpack data)
          }
        | _ -> failwith "Code out of date"
      ;;

      let hierarchical ~name ~size scope =
        let module H = Hierarchy.In_scope (I) (O) in
        H.hierarchical ~scope ~name (create ~size)
      ;;
    end

    module Tests = struct
      module Data = struct
        type 'a t = { test : 'a [@bits 4] } [@@deriving sexp_of, hardcaml]
      end

      open Make (Data)

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
        let sim =
          Simulator.create ~config:Cyclesim.Config.trace_all (create ~size:4 scope)
        in
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
          ((inputs ((clock 0) (clear 0) (push 0) (pop 0) (write_data ((test 0000)))))
           (outputs ((data ((test 0000)))))
           (internals ((entries 00) (mem 0000) (vdd 1))))

          ((inputs ((clock 0) (clear 0) (push 1) (pop 0) (write_data ((test 0001)))))
           (outputs ((data ((test 0001)))))
           (internals ((entries 00) (mem 0000) (vdd 1))))

          ((inputs ((clock 0) (clear 0) (push 0) (pop 0) (write_data ((test 0010)))))
           (outputs ((data ((test 0001)))))
           (internals ((entries 01) (mem 0000) (vdd 1))))

          ((inputs ((clock 0) (clear 0) (push 0) (pop 0) (write_data ((test 0011)))))
           (outputs ((data ((test 0001)))))
           (internals ((entries 01) (mem 0000) (vdd 1))))

          ((inputs ((clock 0) (clear 0) (push 0) (pop 1) (write_data ((test 0100)))))
           (outputs ((data ((test 0000)))))
           (internals ((entries 01) (mem 0000) (vdd 1))))

          ((inputs ((clock 0) (clear 0) (push 0) (pop 0) (write_data ((test 0101)))))
           (outputs ((data ((test 0000)))))
           (internals ((entries 00) (mem 0000) (vdd 1))))

          ((inputs ((clock 0) (clear 0) (push 1) (pop 0) (write_data ((test 0110)))))
           (outputs ((data ((test 0110)))))
           (internals ((entries 00) (mem 0000) (vdd 1))))

          ((inputs ((clock 0) (clear 0) (push 1) (pop 1) (write_data ((test 0111)))))
           (outputs ((data ((test 0111)))))
           (internals ((entries 01) (mem 0000) (vdd 1))))

          ((inputs ((clock 0) (clear 0) (push 1) (pop 1) (write_data ((test 1000)))))
           (outputs ((data ((test 1000)))))
           (internals ((entries 01) (mem 0000) (vdd 1))))

          ((inputs ((clock 0) (clear 0) (push 1) (pop 0) (write_data ((test 1001)))))
           (outputs ((data ((test 1001)))))
           (internals ((entries 01) (mem 0000) (vdd 1))))

          ((inputs ((clock 0) (clear 0) (push 1) (pop 0) (write_data ((test 1010)))))
           (outputs ((data ((test 1010)))))
           (internals ((entries 10) (mem 0000) (vdd 1))))

          ((inputs ((clock 0) (clear 0) (push 1) (pop 0) (write_data ((test 1011)))))
           (outputs ((data ((test 1011)))))
           (internals ((entries 11) (mem 0000) (vdd 1))))

          ((inputs ((clock 0) (clear 0) (push 1) (pop 0) (write_data ((test 1100)))))
           (outputs ((data ((test 1100)))))
           (internals ((entries 00) (mem 0000) (vdd 1))))

          ((inputs ((clock 0) (clear 0) (push 0) (pop 1) (write_data ((test 1101)))))
           (outputs ((data ((test 1011)))))
           (internals ((entries 01) (mem 0000) (vdd 1))))

          ((inputs ((clock 0) (clear 0) (push 0) (pop 1) (write_data ((test 1110)))))
           (outputs ((data ((test 1010)))))
           (internals ((entries 00) (mem 0000) (vdd 1))))

          ((inputs ((clock 0) (clear 0) (push 0) (pop 1) (write_data ((test 1111)))))
           (outputs ((data ((test 1001)))))
           (internals ((entries 11) (mem 0000) (vdd 1))))

          ((inputs ((clock 0) (clear 0) (push 0) (pop 1) (write_data ((test 0000)))))
           (outputs ((data ((test 1100)))))
           (internals ((entries 10) (mem 0000) (vdd 1))))

          ((inputs ((clock 0) (clear 0) (push 0) (pop 1) (write_data ((test 0001)))))
           (outputs ((data ((test 1011)))))
           (internals ((entries 01) (mem 0000) (vdd 1)))) |}]
      ;;
    end
  end

  module Entry = struct
    type 'a t = { return_pc : 'a [@bits Parameters.word_width] }
    [@@deriving sexp_of, hardcaml]
  end

  include Overflowable_stack.Make (Entry)

  let hierarchical = hierarchical ~name:"return_address_stack" ~size:32
end
