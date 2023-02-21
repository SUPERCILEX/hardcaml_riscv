#![no_std]
#![no_main]

use core::{
    arch::global_asm,
    mem::{transmute, MaybeUninit},
};

use bootloader::{decode, Command, COMMAND_BUF_SIZE};
use slib::{
    println,
    stdio::{read_buf, read_byte},
};

fn main() -> ! {
    loop {
        println!("Waiting for commands...");
        let mut command_buf = [MaybeUninit::uninit(); COMMAND_BUF_SIZE];
        read_buf(command_buf.as_mut());
        let command_buf = unsafe { transmute::<_, &[u8; COMMAND_BUF_SIZE]>(&command_buf) };

        let mut arg_buf = [MaybeUninit::uninit(); 8];
        match decode(command_buf, &mut arg_buf) {
            Some(command) => {
                println!("Executing command: {:?}", command);
                match command {
                    Command::Load {
                        into_address,
                        len,
                        checksum,
                    } => {
                        let base_addr = into_address as *mut u8;
                        let mut computed_checksum = 0;
                        for i in 0..usize::try_from(len).unwrap() {
                            let byte = read_byte();
                            computed_checksum += u64::from(byte);
                            unsafe { base_addr.add(i).write(byte) };
                        }

                        if checksum != computed_checksum {
                            println!("Checksum mismatch: {} != {}", checksum, computed_checksum);
                        } else {
                            println!("Transfer complete.")
                        }
                    }
                    Command::Start { address, args } => {
                        let entry =
                            unsafe { transmute::<_, fn(&[&str]) -> usize>(address as *const u8) };
                        println!("Exit code: {}", entry(args));
                    }
                }
            }
            None => {
                println!("Failed to decode command.");
            }
        }
    }
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
