#![no_std]
#![no_main]

use libm::pow;
use slib::println;

#[cfg_attr(not(test), export_name = "_start")]
#[cfg_attr(not(test), link_section = ".start")]
fn main(args: &[&str]) -> i8 {
    println!("pi({}) = {}", args[0], pi(args[0].parse().unwrap()));
    0
}

fn bbp(k: u32) -> f64 {
    let a1 = 4. / f64::from(8 * k + 1);
    let a2 = 2. / f64::from(8 * k + 4);
    let a3 = 1. / f64::from(8 * k + 5);
    let a4 = 1. / f64::from(8 * k + 6);

    (a1 - a2 - a3 - a4) / pow(f64::from(16), f64::from(k))
}

fn pi(n: u32) -> f64 {
    let mut result: f64 = 0.;
    for i in 0..n {
        result += bbp(i);
    }
    result
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
        main(&["10"]);
        main(&["100"]);
    }
}
