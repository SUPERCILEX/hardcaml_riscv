.globl fib
fib:
li t0, 1
bge t0, a0, base_case

addi sp, sp, -16

addi t0, a0, -2
sw t0, (sp)

addi a0, a0, -1
sd ra, 8(sp)
call fib
ld ra, 8(sp)
sw a0, 4(sp)

lw a0, (sp)
sd ra, 8(sp)
call fib
ld ra, 8(sp)
lw t0, 4(sp)
add a0, a0, t0

addi sp, sp, 16
ret

base_case:
ret
