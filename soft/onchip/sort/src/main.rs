#![feature(maybe_uninit_slice)]
#![no_std]
#![no_main]

use core::{mem::MaybeUninit, str::from_utf8};

use slib::{println, stdio::read_byte};

#[cfg_attr(not(test), export_name = "_start")]
#[cfg_attr(not(test), link_section = ".start")]
fn main(args: &[&str]) -> i8 {
    let count = args[0].parse().unwrap();

    let mut array = [MaybeUninit::uninit(); 128];
    load_array(&mut array[..count]);
    let array = unsafe { MaybeUninit::slice_assume_init_mut(&mut array[..count]) };

    array.sort_unstable();
    println!("{:?}", array);

    0
}

fn load_array(into: &mut [MaybeUninit<i64>]) {
    for elem in into {
        let mut str = [MaybeUninit::uninit(); 21];
        let mut i = 0;
        loop {
            let byte = read_byte();
            if byte == b'\n' {
                break;
            }

            str[i].write(byte);
            i += 1;
        }
        elem.write(
            from_utf8(unsafe { MaybeUninit::slice_assume_init_ref(&str[..i]) })
                .unwrap()
                .parse()
                .unwrap(),
        );
    }
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
        test(&["12"]);
    }

    fn test(args: &[&str]) {
        main(black_box(args));
    }
}
