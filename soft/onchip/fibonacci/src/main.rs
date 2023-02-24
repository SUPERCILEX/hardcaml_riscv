#![no_std]
#![no_main]

use slib::println;

#[cfg_attr(not(test), export_name = "_start")]
#[cfg_attr(not(test), link_section = ".start")]
fn main(args: &[&str]) -> i8 {
    if args.len() != 1 {
        return -1;
    }

    println!(
        "Fibonacci of {}: {}",
        args[0],
        fibonacci(args[0].parse().unwrap())
    );

    0
}

fn fibonacci(n: u32) -> u32 {
    if n <= 1 {
        n
    } else {
        fibonacci(n - 1) + fibonacci(n - 2)
    }
}

#[cfg(test)]
mod tests {
    use core::arch::global_asm;

    use crate::main;

    global_asm!(
        r#"
        .pushsection .start, "ax"
        .globl _start
        _start:
            li sp, 0x80000000
            j {}
        .popsection
        "#,
        sym runtime
    );

    fn runtime() {
        main(&["11"]);
    }
}
