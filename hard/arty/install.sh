#!/usr/bin/env bash
set -e

export BOARD="arty-a7-100"

source ~/xilinx/Vivado/2022.2/settings64.sh
dune build --auto-promote
make build
djtgcfg prog --file arty/outputs/top.bit -d Arty -i 0
