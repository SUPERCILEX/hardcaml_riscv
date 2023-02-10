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
             (module Top.I)
             (module Top.O)
             ~name:"top"
             (Top.create scope)))
;;

let command = Command.group ~summary:"Arty board tools" [ "compile", compile ]
let () = Command_unix.run command
