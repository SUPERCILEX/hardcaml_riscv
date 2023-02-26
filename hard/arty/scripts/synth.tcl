read_ip -verbose ips/clk_wiz_0/clk_wiz_0.xci
read_ip -verbose ips/axi_uartlite_0/axi_uartlite_0.xci

read_verilog [ glob *.v ]

upgrade_ip [get_ips]
set_property generate_synth_checkpoint false [get_files ips/clk_wiz_0/clk_wiz_0.xci]
set_property generate_synth_checkpoint false [get_files ips/axi_uartlite_0/axi_uartlite_0.xci]
generate_target all [get_ips]
validate_ip [get_ips]

synth_design -flatten_hierarchy full -retiming -top top
