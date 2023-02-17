open! Core

let waves =
  Command.basic
    ~summary:"Open waveform viewer"
    Command.Let_syntax.(
      let%map_open cycles =
        flag
          ~aliases:[ "-n" ]
          ~doc:"Number of cycles to simulate"
          "-cycles"
          (required int)
      and program =
        flag
          ~doc:"Program to simulate"
          "-program"
          (required
             (Arg_type.enumerated_sexpable
                ~case_sensitive:false
                (module Cpu.Bootloader.For_testing.Sample_programs)))
      in
      fun () ->
        Cpu.Tests.waves ~program ~cycles (fun ~display_rules waves ->
          Hardcaml_waveterm_interactive.run ~signals_width:30 ~display_rules waves))
;;

let command = Command.group ~summary:"Hardware dev tools" [ "waves", waves ]
let () = Command_unix.run command
