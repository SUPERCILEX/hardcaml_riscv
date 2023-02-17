source scripts/setup.tcl
source scripts/synth.tcl
source scripts/place.tcl
source scripts/route.tcl

write_bitstream -force top.bit
