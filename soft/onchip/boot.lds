SECTIONS {
  . = 0x4000;
  .text : {
    *(.start);
    KEEP(*(.text.runtime));
    runtime = .;
    *(.text*);
    LONG(0);
  }
  .rodata : { *(.rodata*) }
  .comment : {}
  .riscv.attributes : {}
}
