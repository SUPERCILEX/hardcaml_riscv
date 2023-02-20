#![no_std]

pub use stdio::_print;

mod stdio;

#[panic_handler]
fn panic(info: &core::panic::PanicInfo) -> ! {
    println!("{}", info);
    loop {}
}
