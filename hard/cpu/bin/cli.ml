open! Core

let waves =
  Command.basic
    ~summary:"Open waveform viewer"
    Command.Let_syntax.(
      return (fun () -> Hardcaml_waveterm_interactive.run (Cpu.Tests.waves ())))
;;

let command = Command.group ~summary:"Hardware dev tools" [ "waves", waves ]
let () = Command_unix.run command
