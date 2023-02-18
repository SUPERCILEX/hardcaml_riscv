SECTIONS {
  . = 0x4000;
  .text : AT (0) {
    *(.text)
    LONG(0);
  }
  .riscv.attributes : {}
  .note.gnu.build-id : {}
}
