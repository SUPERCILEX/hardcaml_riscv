open! Core

module Programs = struct
  type t =
    | Sample of Sample_programs.t
    | Custom
  [@@deriving sexp_of, compare, enumerate]
end

module Shared_commands = struct
  type t =
    { cycles : int option
    ; program : Programs.t
    ; file_name : string option
    ; input_data_file : string option
    ; output_data_file : string option
    ; verilator : Hardcaml_verilator.Simulation_backend.t
    }

  let args () =
    Command.Let_syntax.(
      let%map_open cycles =
        flag
          ~aliases:[ "-n" ]
          ~doc:"N Number of cycles to simulate"
          "-cycles"
          (optional int)
      and program =
        flag
          ~doc:"NAME Program to simulate"
          "-program"
          (required
             (Arg_type.enumerated_sexpable ~case_sensitive:false (module Programs)))
      and file_name =
        flag ~doc:"FILE Binary program file" "-binary" (optional Filename_unix.arg_type)
      and input_data_file =
        flag
          ~doc:"FILE UART input stream file"
          "-input-data-file"
          (optional Filename_unix.arg_type)
      and output_data_file =
        flag
          ~doc:"FILE UART output stream file"
          "-output-data-file"
          (optional Filename_unix.arg_type)
      and verilator = Hardcaml_verilator.Simulation_backend.flag in
      { cycles; program; file_name; input_data_file; output_data_file; verilator })
  ;;

  let program program file_name =
    match program with
    | Programs.Custom ->
      In_channel.with_file
        ~binary:true
        (Option.value_exn ~message:"Supply the program binary with -binary" file_name)
        ~f:In_channel.input_all
    | Sample program -> Sample_programs.program_bytes program
  ;;
end

let waves =
  Command.basic
    ~summary:"Open waveform viewer"
    Command.Let_syntax.(
      let%map_open { Shared_commands.cycles
                   ; program
                   ; file_name
                   ; input_data_file
                   ; output_data_file
                   ; verilator
                   }
        =
        Shared_commands.args ()
      and start_cycle = flag ~doc:"N Start cycle" "-start-cycle" (optional int) in
      fun () ->
        Harness.waves
          ~program:(Shared_commands.program program file_name)
          ~verilator
          ~f:(fun ~display_rules waves ->
            Hardcaml_waveterm_interactive.run
              ~signals_width:30
              ?start_cycle
              ~wave_width:5
              ~display_rules
              waves)
          ?cycles
          ?input_data_file
          ?output_data_file
          ())
;;

let execute =
  Command.basic
    ~summary:"Simulate execution"
    Command.Let_syntax.(
      let%map_open { Shared_commands.cycles
                   ; program
                   ; file_name
                   ; input_data_file
                   ; output_data_file
                   ; verilator
                   }
        =
        Shared_commands.args ()
      in
      fun () ->
        Harness.execute
          ~program:(Shared_commands.program program file_name)
          ~verilator
          ?cycles
          ?input_data_file
          ?output_data_file
          ())
;;

let simulate =
  Command.basic
    ~summary:"Simulate execution with cycle state dump"
    Command.Let_syntax.(
      let%map_open { Shared_commands.cycles
                   ; program
                   ; file_name
                   ; input_data_file
                   ; output_data_file
                   ; verilator
                   }
        =
        Shared_commands.args ()
      in
      fun () ->
        Harness.sim
          ~program:(Shared_commands.program program file_name)
          ~verilator
          ?cycles
          ?input_data_file
          ?output_data_file
          ())
;;

let locate =
  Command.basic
    ~summary:"Trace the origin of a net to hardcaml"
    Command.Let_syntax.(
      let%map_open name =
        flag
          ~doc:"CIRCUIT-NAME Name of the circuit/module"
          "-circuit-name"
          (required string)
      and uid = anon ("UID" %: int) in
      fun () ->
        let open Hardcaml in
        let circuits =
          Caller_id.set_mode Full_trace;
          let scope = Scope.create () in
          let circuit =
            let module C = Circuit.With_interface (Cpu.I) (Cpu.O) in
            C.create_exn ~name:"top" (Cpu.hierarchical scope)
          in
          circuit :: (Scope.circuit_database scope |> Circuit_database.get_circuits)
          |> List.map ~f:(fun c -> Circuit.name c, Circuit.signal_map c)
        in
        match List.Assoc.find circuits name ~equal:String.equal with
        | None -> raise_s [%message "No such circuit"]
        | Some circuit ->
          (match Map.find circuit (Int64.of_int uid) with
           | None -> raise_s [%message "No such uid in circuit"]
           | Some signal -> print_s [%message (signal : Signal.t)]))
;;

let command =
  Command.group
    ~summary:"Hardware dev tools"
    [ "waves", waves; "execute", execute; "simulate", simulate; "locate-net", locate ]
;;

let () = Command_unix.run command
