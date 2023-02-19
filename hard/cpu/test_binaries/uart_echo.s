li a0, 0x2003

loop:
  lb t0, (a0)
  sb t0, (a0)
  bnez t0, loop
