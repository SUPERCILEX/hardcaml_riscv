#!/usr/bin/env bash

[ ! -e "/tmp/command.in" ] && mkfifo "/tmp/command.in"
[ ! -e "/tmp/command.out" ] && mkfifo "/tmp/command.out"

./hold_fd.py "/tmp/command.in" r &
./hold_fd.py "/tmp/command.out" w &
./hold_fd.py "/tmp/command.out" r &
./hold_fd.py "/tmp/command.in" w &

trap : INT
../../../hard/cpu/bin/cli.exe execute -c $1 -p custom -b ../../../_build/default/soft/boot.bin -input /tmp/command.in | tee /tmp/command.out
kill -9 $(jobs -p)
