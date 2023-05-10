open! Core
open Hardcaml
open Cpu

let create ~program ~verilator =
  let scope = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () in
  match verilator with
  | Hardcaml_verilator.Simulation_backend.Hardcaml ->
    let module Simulator = Cyclesim.With_interface (I) (O) in
    Simulator.create ~config:Cyclesim.Config.trace_all (create scope ~bootloader:program)
  | Verilator { cache_dir } ->
    let module Simulator = Hardcaml_verilator.With_interface (I) (O) in
    Simulator.create
      ~verbose:true
      ?cache_dir
      ~clock_names:[ "clock" ]
      (create scope ~bootloader:program)
;;

let test_bench ~step ?input_data_file ?output_data_file (sim : (_ I.t, _ O.t) Cyclesim.t) =
  let open Bits in
  let uart_input =
    Option.value input_data_file ~default:"/dev/null" |> In_channel.create ~binary:true
  in
  let uart_output =
    Option.map output_data_file ~f:(Out_channel.create ~binary:true)
    |> Option.value ~default:stdout
  in
  let inputs, outputs = Cyclesim.inputs sim, Cyclesim.outputs sim in
  let clear () =
    Cyclesim.reset sim;
    inputs.clear := vdd;
    Cyclesim.cycle sim;
    inputs.clear := gnd;
    ()
  in
  clear ();
  let rec run i =
    let read_done = i % 11 = 0 && to_bool !(outputs.uart.read_ready) in
    let write_done = i % 17 = 0 && to_bool !(outputs.uart.write_ready) in
    inputs.uart.read_done := if read_done then vdd else gnd;
    inputs.uart.write_done := if write_done then vdd else gnd;
    inputs.uart.read_data
      := (if read_done then In_channel.input_char uart_input else None)
         |> Option.value ~default:(Char.of_int_exn 0)
         |> of_char;
    if write_done
    then (
      to_int !(outputs.uart.write_data)
      |> Char.of_int_exn
      |> Out_channel.output_char uart_output;
      Out_channel.flush uart_output)
    else ();
    Cyclesim.cycle sim;
    if step i then () else run (i + 1)
  in
  run 1;
  ()
;;

let prettify_enum ~sim ~(enums : 'a list) ~signal_name : 'a =
  let open Bits in
  List.nth_exn
    enums
    (to_int
       !(List.Assoc.find_exn
           (Cyclesim.internal_ports sim)
           signal_name
           ~equal:String.equal))
;;

let max_cycles_or_error max_cycles error cycles =
  let open Bits in
  match max_cycles with
  | Some max_cycles -> max_cycles = cycles
  | None -> to_bool !error
;;

let sim ~program ~verilator ?cycles ?input_data_file ?output_data_file () =
  let sim = create ~program ~verilator in
  let inputs, outputs = Cyclesim.inputs sim, Cyclesim.outputs sim in
  test_bench sim ?input_data_file ?output_data_file ~step:(fun i ->
    let open Bits in
    let all_signals =
      Cyclesim.internal_ports sim
      |> List.filter_map ~f:(fun (signal_name, signal) ->
           (if String.is_substring ~substring:"clock" signal_name
               || String.is_substring ~substring:"clear" signal_name
            then None
            else if (String.is_substring ~substring:"memory_controller$i$" signal_name
                     && String.is_substring
                          ~substring:"memory_controller$i$uart"
                          signal_name
                        |> not)
                    || String.is_substring ~substring:"register_file$i$" signal_name
            then to_int !signal |> Printf.sprintf "0x%x" |> String.sexp_of_t |> Some
            else None)
           |> Option.map ~f:(fun s -> signal_name, Sexp.to_string s))
      |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
    in
    if Cyclesim.internal_ports sim
       |> List.filter_map ~f:(fun (signal_name, signal) ->
            if [ "register_file$i$store"; "memory_controller$i$store" ]
               |> List.map ~f:(fun s -> String.is_substring ~substring:s signal_name)
               |> List.reduce_exn ~f:( || )
            then Some signal
            else None)
       |> List.map ~f:(Fn.compose to_bool ( ! ))
       |> List.reduce_exn ~f:( || )
    then
      Stdio.print_s
        ([%sexp_of: int I.t * int O.t * (string * string) list]
           ( I.map inputs ~f:(fun p -> to_int !p)
           , O.map outputs ~f:(fun p -> to_int !p)
           , all_signals ));
    max_cycles_or_error cycles outputs.error i);
  ()
;;

let execute ~program ~verilator ?cycles ?input_data_file ?output_data_file () =
  let sim = create ~program ~verilator in
  let outputs = Cyclesim.outputs sim in
  test_bench
    sim
    ~step:(max_cycles_or_error cycles outputs.error)
    ?input_data_file
    ?output_data_file;
  let () =
    let out =
      Option.map output_data_file ~f:(Out_channel.create ~append:true)
      |> Option.value ~default:Out_channel.stdout
    in
    Out_channel.output_string
      out
      "\n\n============================================================\n\n";
    Sexp.to_string_hum
      [%message "" ~_:(outputs |> O.map ~f:(fun p -> Bits.to_int !p) : int O.t)]
    |> Out_channel.output_string out;
    ()
  in
  ()
;;

let waves ~program ~verilator ~f ?cycles ?input_data_file ?output_data_file () =
  let open Hardcaml_waveterm in
  let sim = create ~program ~verilator in
  let waves, sim = Waveform.create sim in
  test_bench
    sim
    ~step:
      (let outputs = Cyclesim.outputs sim in
       max_cycles_or_error cycles outputs.error)
    ?input_data_file
    ?output_data_file;
  let open Hardcaml_waveterm.Display_rule in
  let input_rules =
    I.(map port_names ~f:(port_name_is ~wave_format:(Bit_or Hex)) |> to_list)
  in
  let output_rules =
    O.(map port_names ~f:(port_name_is ~wave_format:(Bit_or Hex)) |> to_list)
    @ [ port_name_matches
          ~wave_format:
            (Index
               Cpu.Instruction.(
                 List.map All.all ~f:(All.sexp_of_t |> Fn.compose Sexp.to_string)))
          (let open Re in
           seq [ str "instruction_"; rep alnum; str "_variant" ] |> compile)
      ; (port_name_matches
           ~wave_format:
             (Index
                Cpu.Memory_controller.Size.(
                  List.map Enum.all ~f:(Enum.sexp_of_t |> Fn.compose Sexp.to_string))))
          (let open Re in
           seq [ str "data_size_"; rep alnum; str "_variant" ] |> compile)
      ]
  in
  f ~display_rules:(input_rules @ output_rules @ [ default ]) waves;
  ()
;;
