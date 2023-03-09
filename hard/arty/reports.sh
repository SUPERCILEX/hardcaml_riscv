#!/usr/bin/env bash
set -e

export BOARD="arty-a7-100"

dune build @reports --auto-promote
