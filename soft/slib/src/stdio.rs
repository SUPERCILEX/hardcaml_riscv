use core::fmt;

use spin::mutex::Mutex;

#[macro_export]
macro_rules! println {
    () => (print!("\n"));
    ($($arg:tt)*) => ($crate::print!("{}\n", format_args!($($arg)*)));
}

#[macro_export]
macro_rules! print {
    ($($arg:tt)*) => ($crate::_print(format_args!($($arg)*)));
}

#[doc(hidden)]
pub fn _print(args: fmt::Arguments) {
    use core::fmt::Write;
    static WRITER: Mutex<Writer> = Mutex::new(Writer);
    WRITER.lock().write_fmt(args).unwrap();
}

struct Writer;

impl fmt::Write for Writer {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        const UART_ADDR: *mut u8 = 0x2003 as *mut u8;
        for byte in s.as_bytes() {
            unsafe { UART_ADDR.write_unaligned(*byte) };
        }
        Ok(())
    }
}
