#![feature(maybe_uninit_uninit_array_transpose)]
#![feature(maybe_uninit_slice)]
#![feature(ptr_from_ref)]
#![no_std]

use core::{
    mem::{size_of, transmute, MaybeUninit},
    ptr, str,
};

pub const COMMAND_BUF_SIZE: usize = size_of::<RawCommand>();
const ARG_BUF_SIZE: usize = 128;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Command<'a> {
    Load {
        into_address: u64,
        len: u64,
        checksum: u64,
    },
    Start {
        address: u64,
        args: &'a [&'a str],
    },
}

#[repr(C)]
enum RawCommand {
    Load {
        into_address: u64,
        len: u64,
        checksum: u64,
    },
    Start {
        args: [u8; ARG_BUF_SIZE],
        address: u64,
    },
}

#[must_use]
pub fn encode(command: Command) -> Option<[u8; COMMAND_BUF_SIZE]> {
    let raw_command = match command {
        Command::Load {
            into_address,
            len,
            checksum,
        } => RawCommand::Load {
            into_address,
            len,
            checksum,
        },
        Command::Start { address, args } => {
            let mut args_buf = [MaybeUninit::<u8>::uninit(); ARG_BUF_SIZE];
            {
                let mut args_buf = &mut args_buf[..];
                for arg in args {
                    if arg.len() + 1 > args_buf.len() {
                        return None;
                    }

                    unsafe {
                        ptr::copy_nonoverlapping(
                            arg.as_ptr(),
                            args_buf.as_mut_ptr().cast(),
                            arg.len(),
                        );
                    }
                    args_buf[arg.len()].write(0);
                    args_buf = &mut args_buf[arg.len() + 1..];
                }

                if args_buf.is_empty() {
                    return None;
                }
                args_buf[0].write(0);
            }

            RawCommand::Start {
                args: unsafe { args_buf.transpose().assume_init() },
                address,
            }
        }
    };

    unsafe { Some(transmute(raw_command)) }
}

pub fn decode<'a>(
    raw_command: &'a [u8; COMMAND_BUF_SIZE],
    buf: &'a mut [MaybeUninit<&'a str>],
) -> Option<Command<'a>> {
    Some(
        #[allow(clippy::cast_ptr_alignment)]
        match *unsafe { &*ptr::from_ref(raw_command).cast::<RawCommand>() } {
            RawCommand::Load {
                into_address,
                len,
                checksum,
            } => Command::Load {
                into_address,
                len,
                checksum,
            },
            RawCommand::Start { ref args, address } => {
                let mut arg_count = 0;
                {
                    let mut start = args.as_slice();
                    let mut arg_len = 0;
                    for (byte, next_byte) in args.iter().zip(&args[1..]) {
                        if *byte == 0 {
                            if arg_count == 0 && arg_len == 0 {
                                break;
                            }

                            if arg_count >= buf.len() {
                                return None;
                            }
                            buf[arg_count].write(str::from_utf8(&start[..arg_len]).ok()?);
                            arg_count += 1;

                            if *next_byte == 0 {
                                break;
                            }

                            start = &start[arg_len + 1..];
                            arg_len = 0;
                        } else {
                            arg_len += 1;
                        }
                    }
                }

                Command::Start {
                    address,
                    args: unsafe { MaybeUninit::slice_assume_init_ref(&(buf[..arg_count])) },
                }
            }
        },
    )
}

#[cfg(test)]
mod tests {
    use core::mem::MaybeUninit;

    use crate::{decode, encode, Command};

    #[test]
    fn encode_decode_load() {
        assert_serde(Command::Load {
            into_address: 0x1234,
            len: 0x5678,
            checksum: 0x9abc,
        });
    }

    #[test]
    fn encode_decode_start() {
        assert_serde(Command::Start {
            address: 0x1234,
            args: &["arg1", "arg2"],
        });
    }

    #[test]
    fn encode_decode_start_empty() {
        assert_serde(Command::Start {
            address: 0x1234,
            args: &[],
        });
    }

    fn assert_serde(command: Command) {
        let encoded = encode(command).unwrap();
        let mut buf = [MaybeUninit::uninit(); 2];
        let decoded = decode(&encoded, &mut buf).unwrap();

        assert_eq!(command, decoded);
    }
}
