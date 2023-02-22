#!/usr/bin/env python3

import sys
from threading import Event

fd = open(sys.argv[1], sys.argv[2])
Event().wait()
