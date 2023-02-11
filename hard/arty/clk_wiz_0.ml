open Hardcaml

module I = struct
  type 'a t =
    { clock : 'a [@rtlname "clk_in1"]
    ; reset : 'a [@rtlname "resetn"]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { locked : 'a
    ; clock_10_mhz : 'a [@rtlname "clk_out1"]
    ; clock_50_mhz : 'a [@rtlname "clk_out2"]
    ; clock_100_mhz : 'a [@rtlname "clk_out3"]
    ; clock_166_mhz : 'a [@rtlname "clk_out4"]
    }
  [@@deriving sexp_of, hardcaml]
end

let create (input : _ I.t) =
  let module Instance = Instantiation.With_interface (I) (O) in
  Instance.create ~name:"clk_wiz_0" input
;;
