open! Core

module Programs = struct
  type t =
    | Sample of Sample_programs.t
    | Custom
  [@@deriving sexp_of, compare, enumerate]
end

let waves =
  Command.basic
    ~summary:"Open waveform viewer"
    Command.Let_syntax.(
      let%map_open cycles =
        flag
          ~aliases:[ "-n" ]
          ~doc:"N Number of cycles to simulate"
          "-cycles"
          (required int)
      and start_cycle = flag ~doc:"N Start cycle" "-start-cycle" (optional int)
      and program =
        flag
          ~doc:"NAME Program to simulate"
          "-program"
          (required
             (Arg_type.enumerated_sexpable ~case_sensitive:false (module Programs)))
      and file_name =
        flag ~doc:"FILE Binary program file" "-binary" (optional Filename_unix.arg_type)
      and uart_data_file =
        flag
          ~doc:"FILE UART input stream file"
          "-uart-data-file"
          (optional Filename_unix.arg_type)
      in
      fun () ->
        Cpu.Tests.waves
          ~program:
            (match program with
             | Custom ->
               In_channel.with_file
                 ~binary:true
                 (Option.value_exn
                    ~message:"Supply the program binary with -binary"
                    file_name)
                 ~f:In_channel.input_all
             | Sample program -> Sample_programs.program_bytes program)
          ~cycles
          ?uart_data:
            (Option.map uart_data_file ~f:(fun s ->
               In_channel.with_file ~binary:true s ~f:In_channel.input_all
               |> String.to_list))
          (fun ~display_rules waves ->
            Hardcaml_waveterm_interactive.run
              ~signals_width:30
              ?start_cycle
              ~wave_width:5
              ~display_rules
              waves))
;;

let command = Command.group ~summary:"Hardware dev tools" [ "waves", waves ]
let () = Command_unix.run command
