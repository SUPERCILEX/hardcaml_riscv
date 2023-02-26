source scripts/setup.tcl
source scripts/synth.tcl
source scripts/place.tcl
source scripts/route.tcl

report_bus_skew -file bus_skew.rpt
report_clock_utilization -file clock_utilization.rpt
report_design_analysis -complexity -congestion -timing -setup -hold -show_all -extend -qor_summary -file design_analysis.rpt
report_drc -file drc.rpt
report_high_fanout_nets -file high_fanout_nets.rpt
report_methodology -file methodology.rpt
report_power -file power.rpt 
report_qor_suggestions -file qor_suggestions.rpt
report_ram_utilization -file ram_utilization.rpt 
report_timing_summary -file timing_summary.rpt
report_utilization -file utilization.rpt
