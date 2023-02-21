li sp, 0x80000000
addi sp, sp, -240
li a0, 42
sw a0, 68(sp)
addi a1, sp, 68
lw a0, (a1)
addi a1, a1, 68
lw a0, -68(a1)

addi a0, sp, -1052
auipc a2, 0x2

li a0, 2
loop:
addi a0, a0, -1
bnez a0, loop

li a0, 0x12345678
sw a0, (sp)
sb a2, (sp)
lw a0, (sp)

j after
li a0, 0
after:
mv a0, a0

auipc a0, 0
addi a0, a0, 0x10
jalr ra, (a0)
li a0, 0
mv a0, a0

li a0, 0b11100011010100010
li a1, 0b01110001101010001
and a2, a0, a1
andi a2, a0, 0b00000011100011010
xori a2, a0, 0b00000011100011010
li a3, -0x1000
slti a3, a3, -64

li a0, -1
sw a0, (sp)
lbu a0, (sp)
lb a0, (sp)
