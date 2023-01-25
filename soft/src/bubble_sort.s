init:
# arr = {6, 1, 3, 2, 7}
LI a0, 0x10000000 # start address
LI a2, 6
SB a2,0(a0)
LI a2, 1
SB a2,4(a0)
LI a2, 3
SB a2,8(a0)
LI a2, 2
SB a2,12(a0)
LI a2, 7
SB a2,16(a0)
# BEGIN YOUR CODE HERE

li a1, 1

# a0=array addr, a1=array len
bubbleSort:
li a2, 1
ble a1, a2, exit

# a3 = swapped, a4=j
outer:
li a3, 0
li a4, 0

# a5 = list[j], a6=list[j+1], a7=j pointer
inner:
slli a7, a4, 2
lw a5, 0(a7)
lw a6, 4(a7)
ble a5, a6, inner_epilogue

li a3, 1
swap:
sw a5, 4(a7)
sw a6, 0(a7)

inner_epilogue:
addi a4, a4, 1
blt a4, a1, inner

maybe_break:
bne a3, zero, outer

exit:
li a0, 10 # Code for exit ecall
ecall
