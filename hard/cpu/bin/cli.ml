open! Core
open Hardcaml

let compile =
  Command.basic
    ~summary:"Compile into verilog"
    Command.Let_syntax.(
      let%map_open filename =
        flag ~doc:"Output file" "-output" (optional Filename_unix.arg_type)
      in
      fun () ->
        let scope = Scope.create () in
        Rtl.output
          ~output_mode:
            (match filename with
             | None -> Rtl.Output_mode.To_channel Stdio.Out_channel.stdout
             | Some file -> Rtl.Output_mode.To_file file)
          ~database:(Scope.circuit_database scope)
          Verilog
          (Circuit.create_with_interface
             (module Cpu.I)
             (module Cpu.O)
             ~name:"cpu_top"
             (Cpu.circuit scope)))
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
