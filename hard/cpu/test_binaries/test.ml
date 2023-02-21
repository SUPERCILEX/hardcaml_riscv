open! Core

let simulate =
  Command.basic
    ~summary:"CPU signal tests"
    Command.Let_syntax.(
      let%map_open cycles =
        flag ~doc:"N Number of cycles to simulate" "-cycles" (required int)
      and program =
        flag
          ~doc:"NAME Program to simulate"
          "-program"
          (required
             (Arg_type.enumerated_sexpable ~case_sensitive:false (module Sample_programs)))
      and uart_data =
        flag ~doc:"BYTES UART input stream" "-uart-data" (optional string)
      in
      fun () ->
        Cpu.Tests.sim
          ~program:(Sample_programs.program_bytes program)
          ?uart_data:
            (Option.map uart_data ~f:(fun s ->
               String.split s ~on:' '
               |> List.map ~f:(Int.of_string |> Fn.compose Char.of_int_exn)))
          (( = ) cycles))
;;

let () = Command_unix.run simulate
