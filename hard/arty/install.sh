#!/usr/bin/env bash
set -e

export BOARD="arty-a7-100"

source ~/xilinx/Vivado/2022.2/settings64.sh
dune build --auto-promote
djtgcfg prog --file top.bit -d Arty -i 0
