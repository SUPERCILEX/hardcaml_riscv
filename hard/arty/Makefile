outputs/post_synth.dcp: scripts/synth.tcl top.v
	vivado -nojournal -mode batch -source scripts/synth.tcl -log outputs/synth.log -tclargs $(BOARD)

outputs/post_place.dcp: scripts/place.tcl outputs/post_synth.dcp
	vivado -nojournal -mode batch -source scripts/place.tcl -log outputs/place.log

outputs/top.bit outputs/post_route.dcp: scripts/route.tcl outputs/post_place.dcp
	vivado -nojournal -mode batch -source scripts/route.tcl -log outputs/route.log

build: outputs/top.bit

clean:
	rm -rf outputs
	rm -rf .Xil
	rm -f top.v
