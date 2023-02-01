use core::arch::global_asm;

#[cfg(not(target_arch = "x86_64"))]
global_asm!(include_str!("fibonacci.s"));
#[cfg(target_arch = "x86_64")]
global_asm!("fib:");

extern "C" {
    fn fib(n: u32) -> u32;
}

fn main() {
    for n in 0..25 {
        assert_eq!(fib_rust(n), unsafe { fib(n) });
    }
}

fn fib_rust(n: u32) -> u32 {
    if n <= 1 {
        n
    } else {
        fib_rust(n - 1) + fib_rust(n - 2)
    }
}
