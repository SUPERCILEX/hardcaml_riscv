source scripts/setup.tcl
source scripts/synth.tcl
source scripts/place.tcl
source scripts/route.tcl

report_timing_summary -file timing_summary.rpt
report_bus_skew -file bus_skew.rpt
report_utilization -file utilization.rpt
report_qor_suggestions -file suggestions.rpt
report_design_analysis -complexity -congestion -timing -setup -hold -show_all -extend -qor_summary -file design_analysis.rpt
report_power -file power.rpt 
report_ram_utilization -file ram_utilization.rpt 
report_high_fanout_nets -file high_fanout_nets.rpt
