la a0, test_string

strchr:
  beq a0, zero, fail

addi a0, a0, -1
loop:
  addi a0, a0, 1
  lb a2, (a0)
  beq a2, zero, fail

  bne a2, a1, loop

  ret

fail:
  li a0, 0
  ret

.data
test_string:
  .asciz "Hello World!\n"
