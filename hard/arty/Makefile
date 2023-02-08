top.v: arty.exe
	./arty.exe compile -output top.v

outputs/post_synth.dcp: scripts/synth.tcl top.v
	vivado -nojournal -mode batch -source scripts/synth.tcl -tclargs $(BOARD) -log outputs/synth.log

outputs/post_place.dcp: scripts/place.tcl outputs/post_synth.dcp
	vivado -nojournal -mode batch -source scripts/place.tcl -log outputs/place.log

outputs/top.bit outputs/post_route.dcp: scripts/route.tcl outputs/post_place.dcp
	vivado -nojournal -mode batch -source scripts/route.tcl -log outputs/route.log

build: outputs/top.bit

clean:
	rm -f outputs/*
