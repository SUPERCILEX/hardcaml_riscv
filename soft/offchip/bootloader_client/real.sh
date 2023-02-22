#!/usr/bin/env bash

[ ! -e "/tmp/command.out" ] && mkfifo "/tmp/command.out"

./hold_fd.py "/tmp/command.out" r &

trap : INT
cat /dev/ttyUSB1 | tee /tmp/command.out
kill -9 $(jobs -p)
