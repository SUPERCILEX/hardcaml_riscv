open Hardcaml

module I = struct
  type 'a t =
    { clock : 'a [@rtlname "s_axi_aclk"]
    ; reset : 'a [@rtlname "s_axi_aresetn"]
    ; write_address : 'a [@bits 4] [@rtlname "s_axi_awaddr"]
    ; write_address_valid : 'a [@rtlname "s_axi_awvalid"]
    ; write_data : 'a [@bits 32] [@rtlname "s_axi_wdata"]
    ; write_strobe : 'a [@bits 4] [@rtlname "s_axi_wstrb"]
    ; write_data_valid : 'a [@rtlname "s_axi_wvalid"]
    ; write_response_ready : 'a [@rtlname "s_axi_bready"]
    ; read_address : 'a [@bits 4] [@rtlname "s_axi_araddr"]
    ; read_address_valid : 'a [@rtlname "s_axi_arvalid"]
    ; read_response_ready : 'a [@rtlname "s_axi_rready"]
    ; receive : 'a [@rtlname "rx"]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { interrupt : 'a [@rtlname "interrupt"]
    ; write_address_ready : 'a [@rtlname "s_axi_awready"]
    ; write_data_ready : 'a [@rtlname "s_axi_wready"]
    ; write_response : 'a [@bits 2] [@rtlname "s_axi_bresp"]
    ; write_response_valid : 'a [@rtlname "s_axi_bvalid"]
    ; read_address_ready : 'a [@rtlname "s_axi_arready"]
    ; read_data : 'a [@bits 32] [@rtlname "s_axi_rdata"]
    ; read_response : 'a [@bits 2] [@rtlname "s_axi_rresp"]
    ; read_response_valid : 'a [@rtlname "s_axi_rvalid"]
    ; transmit : 'a [@rtlname "tx"]
    }
  [@@deriving sexp_of, hardcaml]
end

let create (input : _ I.t) =
  let module Instance = Instantiation.With_interface (I) (O) in
  Instance.create ~name:"axi_uartlite_0" input
;;
