# Arty

## Setup

1. Get the Digilent Utilities: https://digilent.com/reference/software/adept/start
2. Install Vivado: https://www.xilinx.com/support/download.html

## Building

### Upload

`./upload.sh`

### Reports

`./reports.sh`

## Dev workflow

`dune build @runtest @bin @fmt --auto-promote -w`

### Running sample programs

1. Run `soft/offchip/bootloader_client/real.sh`
2. Inside the `soft/offchip/bootloader_client` directory, run (for example)
   `cargo r -- -o /dev/ttyUSB1 -i /tmp/command.out load ../../onchip/donut/donut.bin`
3. To start the program, run `cargo r -- -o /dev/ttyUSB1 -i /tmp/command.out start`

## Background

Project setup inspired by https://github.com/fyquah/hardcaml_arty. \
Board files taken from https://github.com/Digilent/vivado-boards/tree/master/new/board_files. \
Ports taken from https://github.com/Digilent/Arty/blob/master/Resources/XDC/Arty_Master.xdc. \
IPs generated from temporary Vivado project and then copied into [ips/] with `gen_directory` and
`OUTPUTDIR` set to `.`.
