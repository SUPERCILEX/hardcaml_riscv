la a0, str

atoi:
li t0, 0 # Value
li t1, 1 # Mul
beq a0, zero, done

lb a1, 0(a0) # Get first char
li t2, 43 # +
bne a1, t2, skip1
addi a0, a0, 1

skip1:
lb a1, 0(a0)
li t2, 45 # -
bne a1, t2, skip2
addi a0, a0, 1
li t1, -1

skip2:

loop:
lb a1, 0(a0)
beq a1, zero, done
mv t2, t0
slli t0, t2, 3
add t0, t0, t2
add t0, t0, t2
addi t4, a1, -48 # 0
add t0, t0, t4
addi a0, a0, 1
j loop


done:
mv t3, t0
slli t3, t3, 1
sgtz t4, t1
slli t4, t4, 31
srai t4, t4, 31
and t3, t3, t4
sub a0, t3, t0
ret

.data
str:
  .asciz "-1234"
