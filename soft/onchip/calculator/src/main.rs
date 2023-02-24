#![no_std]
#![no_main]

use slib::println;

#[cfg_attr(not(test), export_name = "_start")]
#[cfg_attr(not(test), link_section = ".start")]
fn main(args: &[&str]) -> i8 {
    let [a, op, b] = args else {
        println!("Usage: <number> <operator> <number>");
        return -1;
    };

    match (a.parse::<i128>().unwrap(), *op, b.parse::<i128>().unwrap()) {
        (a, "+", b) => {
            println!("{a} + {b} = {}", a + b);
        }
        (a, "-", b) => {
            println!("{a} - {b} = {}", a - b);
        }
        (a, "*", b) => {
            println!("{a} * {b} = {}", a * b);
        }
        (a, "/", b) => {
            println!("{a} / {b} = {}", a / b);
        }
        _ => {
            println!("Unknown operator: {op}");
            return -1;
        }
    }
    0
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
        main(&["3958142856", "+", "918912018"]);
        main(&["2647997759", "-", "17199902"]);
        main(&["1666628347", "*", "-3984378142"]);
        main(&["3817954958", "/", "1007979948"]);
    }
}
