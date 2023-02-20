#![no_std]
#![no_main]

use core::arch::asm;

use slib::println;

fn main() {
    println!("Hello, world!");
}

#[no_mangle]
pub extern "C" fn _start() {
    unsafe {
        asm!("li sp, 0x80000000", options(nostack));
    }
    main();
}
