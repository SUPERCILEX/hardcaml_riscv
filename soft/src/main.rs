use core::arch::global_asm;
use std::{ffi::CString, ptr};

#[cfg(not(target_arch = "x86_64"))]
global_asm!(include_str!("atoi.s"));
#[cfg(target_arch = "x86_64")]
global_asm!("atoi:");

extern "C" {
    fn atoi(s: *const u8) -> i32;
}

fn main() {
    for s in [
        "123",
        "-123",
        "+123",
        "0",
        "-0",
        "+0",
        "1234567890",
        "",
        "+-123",
    ] {
        let cstr = CString::new(s).unwrap();
        assert_eq!(atoi_rust(s.as_bytes()), unsafe {
            atoi(cstr.as_ptr() as *const _)
        });
    }
    assert_eq!(0, unsafe { atoi(ptr::null()) });
}

fn atoi_rust(mut s: &[u8]) -> i32 {
    let mut value = 0i32;
    let mut mul = 1;
    if s.is_empty() {
        return 0;
    }

    if s[0] == b'+' {
        s = &s[1..];
    }
    if s[0] == b'-' {
        s = &s[1..];
        mul = -1;
    }
    for s in s {
        value = value * 10 + i32::from(*s - b'0');
    }
    value * mul
}
