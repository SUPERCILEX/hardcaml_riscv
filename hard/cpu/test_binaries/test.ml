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
             (Arg_type.enumerated_sexpable
                ~case_sensitive:false
                (module Cpu.Bootloader.For_testing.Sample_programs)))
      in
      fun () ->
        Cpu.Tests.sim
          ~program:(Cpu.Bootloader.For_testing.sample program)
          ~termination:(( = ) cycles))
;;

let () = Command_unix.run simulate
