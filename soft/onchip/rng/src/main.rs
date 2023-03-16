#![no_std]
#![no_main]

use rand_xoshiro::{
    rand_core::{RngCore, SeedableRng},
    Xoshiro256PlusPlus,
};
use slib::println;

#[cfg_attr(not(test), export_name = "_start")]
#[cfg_attr(not(test), link_section = ".start")]
fn main(args: &[&str]) -> i8 {
    let mut rand = Xoshiro256PlusPlus::seed_from_u64(args[0].parse().unwrap());
    let count = args[1].parse().unwrap();

    for _ in 0..count {
        println!("{}", rand.next_u64());
    }
    0
}

#[cfg(test)]
mod tests {
    use core::{arch::global_asm, hint::black_box};

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
        test(&["42", "100"]);
    }

    fn test(args: &[&str]) {
        main(black_box(args));
    }
}
