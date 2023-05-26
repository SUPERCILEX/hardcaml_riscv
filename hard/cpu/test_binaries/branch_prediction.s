li a0, 50

loop:
andi a1, a0, 1
bne a1, zero, odd
j even

odd:
addi t1, t1, 1
j check

even:
addi t0, t0, 1

check:
addi a0, a0, -1
bnez a0, loop
