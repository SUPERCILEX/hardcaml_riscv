.globl atoi
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
li t3, 10

loop:
lb a1, 0(a0)
beq a1, zero, done
mul t0, t0, t3
addi t4, a1, -48 # 0
add t0, t0, t4
addi a0, a0, 1
j loop

done:
mul a0, t0, t1
ret
