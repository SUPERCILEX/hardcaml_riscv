use core::arch::global_asm;

#[cfg(not(target_arch = "x86_64"))]
global_asm!(include_str!("strchr.s"));
#[cfg(target_arch = "x86_64")]
global_asm!("my_strchr:");

extern "C" {
    fn my_strchr(a: *const u8, b: u8) -> *const u8;
}

fn main() {
    debug(core::ptr::null(), 42);
    debug(b"abc\x42def\0".as_ptr(), 0x42);
    debug(b"abcdef\0".as_ptr(), 0x42);
}

fn debug(ptr: *const u8, target: u8) {
    println!("start: {ptr:?}, end: {:?}", unsafe {
        my_strchr(ptr, target)
    });
}
