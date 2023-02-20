use core::fmt;

#[macro_export]
macro_rules! println {
    () => (print!("\n"));
    ($($arg:tt)*) => ($crate::print!("{}\n", format_args!($($arg)*)));
}

#[macro_export]
macro_rules! print {
    ($($arg:tt)*) => ($crate::stdio::_print(format_args!($($arg)*)));
}

#[doc(hidden)]
pub fn _print(args: fmt::Arguments) {
    use core::fmt::Write;
    static mut WRITER: Writer = Writer;
    unsafe {
        WRITER.write_fmt(args).unwrap();
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
