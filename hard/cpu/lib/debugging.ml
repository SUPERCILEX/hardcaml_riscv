open! Core
open Hardcaml

module In_scope (I : Interface.S) (O : Interface.S) = struct
  let create ~create_fn scope input =
    let output = create_fn scope input in
    let _debugging =
      let ( -- ) = Scope.naming scope in
      I.Of_signal.apply_names input ~naming_op:( -- ) |> ignore;
      O.Of_signal.apply_names output ~naming_op:( -- ) |> ignore
    in
    output
  ;;
end
