#![feature(ptr_from_ref)]
#![no_std]
#![no_main]

use core::{
    arch::global_asm,
    mem::{transmute, MaybeUninit},
    ptr, slice,
};

use bootloader::{decode, Command, COMMAND_BUF_SIZE};
use slib::{println, stdio::read_buf};

fn main() -> ! {
    loop {
        println!("Waiting for commands...");
        let mut command_buf = [MaybeUninit::uninit(); COMMAND_BUF_SIZE];
        read_buf(command_buf.as_mut());
        let command_buf = unsafe { &*ptr::from_ref(&command_buf).cast::<[u8; COMMAND_BUF_SIZE]>() };

        let mut arg_buf = [MaybeUninit::uninit(); 8];
        let Some(command) = decode(command_buf, &mut arg_buf) else {
            println!("Failed to decode command.");
            continue;
        };

        println!("Executing command: {:?}", command);
        match command {
            Command::Load {
                into_address,
                len,
                checksum,
            } => {
                let target = {
                    let len = usize::try_from(len).unwrap();
                    let target = into_address as *mut u8;
                    read_buf(unsafe { slice::from_raw_parts_mut(target.cast(), len) });
                    unsafe { slice::from_raw_parts_mut(target, len) }
                };

                let computed_checksum = target.iter().copied().map(u64::from).sum();
                if checksum == computed_checksum {
                    println!("Transfer complete.");
                } else {
                    println!("Checksum mismatch: {} != {}", checksum, computed_checksum);
                }
            }
            Command::Start { address, args } => {
                let entry = unsafe { transmute::<_, fn(&[&str]) -> i8>(address as *const u8) };
                println!("Exit code: {}", entry(args));
            }
        }
    }
}

global_asm!(
    r#"
    .pushsection .start, "ax"
    .globl _start
    _start:
        li sp, 0x80000000
        j {}
    .popsection
    "#,
    sym main
);
