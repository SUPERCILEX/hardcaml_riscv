open Hardcaml

let () =
  let scope = Scope.create () in
  Rtl.print ~database:(Scope.circuit_database scope) Verilog (Cpu.root scope)
;;