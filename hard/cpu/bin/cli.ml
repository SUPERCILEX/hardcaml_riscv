open! Core

module Programs = struct
  type t =
    | Sample of Sample_programs.t
    | Custom
  [@@deriving sexp_of, compare, enumerate]
end

module Shared_commands = struct
  type t =
    { cycles : int
    ; program : Programs.t
    ; file_name : string option
    ; input_data_file : string option
    ; output_data_file : string option
    }

  let args () =
    Command.Let_syntax.(
      let%map_open cycles =
        flag
          ~aliases:[ "-n" ]
          ~doc:"N Number of cycles to simulate"
          "-cycles"
          (required int)
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
      in
      { cycles; program; file_name; input_data_file; output_data_file })
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
                   }
        =
        Shared_commands.args ()
      and start_cycle = flag ~doc:"N Start cycle" "-start-cycle" (optional int) in
      fun () ->
        Cpu.Tests.waves
          ~program:(Shared_commands.program program file_name)
          ~cycles
          ?input_data_file
          ?output_data_file
          (fun ~display_rules waves ->
          Hardcaml_waveterm_interactive.run
            ~signals_width:30
            ?start_cycle
            ~wave_width:5
            ~display_rules
            waves))
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
                   }
        =
        Shared_commands.args ()
      in
      fun () ->
        Cpu.Tests.execute
          ~program:(Shared_commands.program program file_name)
          ?input_data_file
          ?output_data_file
          cycles)
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
                   }
        =
        Shared_commands.args ()
      in
      fun () ->
        Cpu.Tests.sim
          ~program:(Shared_commands.program program file_name)
          ?input_data_file
          ?output_data_file
          (( = ) cycles))
;;

let command =
  Command.group
    ~summary:"Hardware dev tools"
    [ "waves", waves; "execute", execute; "simulate", simulate ]
;;

let () = Command_unix.run command
