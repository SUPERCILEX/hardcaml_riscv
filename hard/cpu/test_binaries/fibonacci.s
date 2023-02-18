li sp, 0x80000000
li a0, 10

addi sp, sp, -16
sw ra, 8(sp)
call fib
mv a0, a0
lw zero, (zero)

fib:
li t0, 1
bge t0, a0, base_case

addi sp, sp, -16

addi t0, a0, -2
sw t0, (sp)

addi a0, a0, -1
sw ra, 8(sp)
call fib
lw ra, 8(sp)
sw a0, 4(sp)

lw a0, (sp)
sw ra, 8(sp)
call fib
lw ra, 8(sp)
lw t0, 4(sp)
add a0, a0, t0

addi sp, sp, 16
ret

base_case:
ret
