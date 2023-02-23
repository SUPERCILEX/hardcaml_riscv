#!/usr/bin/env bash

[ $(stat -c "%a" /dev/ttyUSB1) -gt 665 ] || sudo chmod 666 /dev/ttyUSB1
(cd ../tstp && cargo r -- -b 128000 /dev/ttyUSB1)

[ ! -e "/tmp/command.out" ] && mkfifo "/tmp/command.out"

./hold_fd.py "/tmp/command.out" r &

trap : INT
cat /dev/ttyUSB1 | tee /tmp/command.out
kill -9 $(jobs -p)
