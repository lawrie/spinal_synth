VERILOG = ../../PdmPlayer.v
PCF = audio.pcf
DEVICE = lp8k 
PACKAGE = cm81
SCALA ?= SongTestBx

prog : bin/toplevel.bin
	tinyprog -p bin/toplevel.bin

bin/toplevel.json : ${VERILOG}
	mkdir -p bin
	rm -f *.bin
	cp ${VERILOG}*.bin . | true
	yosys -v3 -p "synth_ice40 -json bin/toplevel.json" ${VERILOG}

bin/toplevel.asc : ${PCF} bin/toplevel.json
	nextpnr-ice40 --freq 20 --${DEVICE} --package ${PACKAGE} --json bin/toplevel.json --pcf ${PCF} --asc bin/toplevel.asc --opt-timing --placer heap

bin/toplevel.bin : bin/toplevel.asc
	icepack bin/toplevel.asc bin/toplevel.bin

${VERILOG} :
	(cd ../..; sbt "runMain synth.${SCALA}")
  
compile : bin/toplevel.bin

time: bin/toplevel.bin
	icetime -tmd hx8k bin/toplevel.asc

clean :
	rm -rf bin
	rm -f *.bin
	rm ${VERILOG}
	rm -f ${VERILOG}*.bin
