open Core
open Hardcaml

let compile =
  Command.basic
    ~summary:"Compile into verilog"
    Command.Let_syntax.(
      let%map_open filename =
        anon (maybe_with_default "-" ("filename" %: Filename_unix.arg_type))
      in
      fun () ->
        let scope = Scope.create () in
        Rtl.output
          ~output_mode:
            (match filename with
             | "-" -> Rtl.Output_mode.To_channel Stdio.Out_channel.stdout
             | file -> Rtl.Output_mode.To_file file)
          ~database:(Scope.circuit_database scope)
          Verilog
          (Cpu.root scope))
;;

let waves =
  Command.basic
    ~summary:"Open waveform viewer"
    Command.Let_syntax.(
      return (fun () -> Hardcaml_waveterm_interactive.run (Cpu.Tests.waves ())))
;;

let command =
  Command.group ~summary:"Hardware dev tools" [ "compile", compile; "waves", waves ]
;;

let () = Command_unix.run command
