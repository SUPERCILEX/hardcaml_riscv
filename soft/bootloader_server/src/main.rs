#![no_std]
#![no_main]

use core::arch::global_asm;

use slib::println;

fn main() {
    println!("Hello, world!");
}

global_asm!(
    r"
    .pushsection .start

    .globl _start
    _start:
        li sp, 0x80000000

    .popsection
"
);

#[no_mangle]
extern "C" fn runtime() {
    main();
}
