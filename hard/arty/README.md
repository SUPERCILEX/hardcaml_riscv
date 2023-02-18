# Arty

## Setup

1. Get the Digilent Utilities: https://digilent.com/reference/software/adept/start
1. (Optional) Install Vivado: https://www.xilinx.com/support/download.html

## Build/upload

`./upload.sh`

## Dev workflow

`dune build @runtest @fmt --auto-promote -w`

## Background

Project setup inspired by https://github.com/fyquah/hardcaml_arty. \
Board files taken from https://github.com/Digilent/vivado-boards/tree/master/new/board_files. \
Ports taken from https://github.com/Digilent/Arty/blob/master/Resources/XDC/Arty_Master.xdc. \
IPs generated from temporary Vivado project and then copied into [ips/] with `gen_directory` and
`OUTPUTDIR` set to `.`.
