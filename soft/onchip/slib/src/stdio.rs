use core::{fmt, mem::MaybeUninit};

#[macro_export]
macro_rules! println {
    () => (print!("\n"));
    ($($arg:tt)*) => ($crate::print!("{}\n", format_args!($($arg)*)));
}

#[macro_export]
macro_rules! print {
    ($($arg:tt)*) => ($crate::stdio::_print(format_args!($($arg)*)));
}

#[macro_export]
macro_rules! dbg {
    () => {
        $crate::println!("[{}:{}]", file!(), line!());
    };
    ($val:expr) => {
        // Use of `match` here is intentional because it affects the lifetimes
        // of temporaries - https://stackoverflow.com/a/48732525/1063961
        match $val {
            tmp => {
                $crate::println!("[{}:{}] {} = {:#?}",
                    file!(), line!(), stringify!($val), &tmp);
                tmp
            }
        }
    };
    // Trailing comma with single argument is ignored
    ($val:expr,) => { $crate::dbg!($val) };
    ($($val:expr),+ $(,)?) => {
        ($($crate::dbg!($val)),+,)
    };
}

#[doc(hidden)]
pub fn _print(args: fmt::Arguments) {
    use core::fmt::Write;
    static mut WRITER: Writer = Writer;
    unsafe {
        WRITER.write_fmt(args).unwrap();
    }
}

pub fn read_buf(buf: &mut [MaybeUninit<u8>]) {
    for byte in buf {
        byte.write(read_byte());
    }
}

struct Writer;

impl fmt::Write for Writer {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        for byte in s.as_bytes() {
            write_byte(*byte);
        }
        Ok(())
    }
}

const UART_ADDR: *mut u8 = 0x2003 as *mut u8;

pub fn write_byte(byte: u8) {
    unsafe { UART_ADDR.write_volatile(byte) };
}

pub fn read_byte() -> u8 {
    unsafe { UART_ADDR.read_volatile() }
}
