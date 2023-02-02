lui a0, 0xdead
auipc a1, 0xbeef
jal a2, 0xcafe
jalr a3, a0, 0x69
beq a4, a1, 0x42
bne a5, a2, 420
blt a6, a3, 88
bge a7, a4, 1234
bltu t0, a5, -1
bgeu t1, a6, -1
lb t2, 42(a7)
lh t3, 1234(t0)
lw t4, 0x69(t1)
lbu t5, -1(t2)
lhu t6, -1(t3)
sb t2, 42(a7)
sh t3, 1234(t0)
sw t4, 0x69(t1)
addi s0, t4, 987
slti s1, t5, 574
sltiu s2, t6, -1
xori s3, s0, 296
ori s4, s1, 420
andi s5, s2, 698
slli s5, s2, 8
srli s5, s2, 21
srai s5, s2, 17
add s6, s5, s2
sub s6, s5, s2
sll s6, s5, s2
slt s6, s5, s2
sltu s6, s5, s2
xor s6, s5, s2
srl s6, s5, s2
sra s6, s5, s2
or s6, s5, s2
and s6, s5, s2
