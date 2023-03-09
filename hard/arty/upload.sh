#!/usr/bin/env bash
set -e

export BOARD="arty-a7-100"

dune build top.bit --auto-promote
djtgcfg prog --file top.bit -d Arty -i 0
