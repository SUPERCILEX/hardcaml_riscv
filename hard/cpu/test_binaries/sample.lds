SECTIONS {
  . = 0x4000;
  .text 0x4000 : {
    *(.text*)
    LONG(0);
  }
  .note.gnu.build-id : {}
}
