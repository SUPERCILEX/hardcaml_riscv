#!/usr/bin/env bash
set -e

export BOARD="arty-a7-100"

source ~/xilinx/Vivado/2022.2/settings64.sh
dune build @reports --auto-promote
