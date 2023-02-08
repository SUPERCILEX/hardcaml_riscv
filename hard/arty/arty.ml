open! Core
open Hardcaml
open Hardcaml_arty

let create scope (app : _ User_application.I.t) =
  let open Signal in
  let cpu =
    Cpu.circuit scope { Cpu.I.clock = app.clk_200; clear = ~:(app.clear_n_200) }
  in
  { User_application.O.led_4bits = uresize cpu._unused 4
  ; uart_tx = { With_valid.valid = gnd; value = zero 8 }
  ; led_rgb =
      List.init 4 ~f:(fun _ -> { User_application.Led_rgb.r = gnd; g = gnd; b = gnd })
  ; ethernet = User_application.Ethernet.O.unused (module Signal)
  }
;;

let circuit scope =
  let module Top = Rtl_generator.Top in
  Circuit.create_with_interface
    (module Top.I)
    (module Top.O)
    ~name:"top"
    (Top.create ~instantiate_ethernet_mac:false create scope)
;;

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
          (circuit scope))
;;

let command = Command.group ~summary:"Arty board tools" [ "compile", compile ]
let () = Command_unix.run command
