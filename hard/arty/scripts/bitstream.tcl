source scripts/setup.tcl
source scripts/synth.tcl
source scripts/place.tcl
source scripts/route.tcl

if [expr {[get_property SLACK [get_timing_paths -delay_type min_max]] < 0}] { error "ERROR: Timing failed" }
write_bitstream -force top.bit
