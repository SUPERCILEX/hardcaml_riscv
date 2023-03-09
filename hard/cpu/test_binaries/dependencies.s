li a0, 0x42
addi a1, a0, 0x27
add a2, a1, a0
add a2, a0, a1
sub a2, a2, a2
add a2, a0, a2

li t0, 0x2003
lb t1, (t0)
lb t1, (t0)
div a0, a1, a2
rem a1, a1, a2
div a0, a1, a0
