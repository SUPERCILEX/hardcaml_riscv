open! Core

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
             (Arg_type.enumerated_sexpable
                ~case_sensitive:false
                (module Cpu.Bootloader.For_testing.Sample_programs)))
      in
      fun () ->
        Cpu.Tests.waves ~program ~cycles (fun ~display_rules waves ->
          Hardcaml_waveterm_interactive.run
            ~signals_width:30
            ?start_cycle
            ~wave_width:5
            ~display_rules
            waves))
;;

let command = Command.group ~summary:"Hardware dev tools" [ "waves", waves ]
let () = Command_unix.run command
