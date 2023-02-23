SECTIONS {
  . = 0x100000;
  .text : {
    KEEP(*(.start*));
    _start = .;
    *(.text*);
    LONG(0);
  }
  .rodata : { *(.rodata*) }
  .comment : {}
  .riscv.attributes : {}
}
