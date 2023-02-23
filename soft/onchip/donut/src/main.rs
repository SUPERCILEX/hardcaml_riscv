#![no_std]
#![no_main]

use libm::{cosf, sinf};
use slib::print;

#[cfg_attr(not(test), export_name = "_start")]
#[cfg_attr(not(test), link_section = ".start")]
#[rustfmt::skip]
             fn main(){let(
         mut a,mut b,s)=(0.,0.,
       sin_cos);print!("\x1b\
     [2J");loop{let(mut r,mut z,mut
   j)=([' ';1760],[0.;1760],0.);while
  j<6.28{let mut i=0.;while i<6.28{let
 ((c,l),(f,d),(e,g),(n,m))=(s(i),s(j),s
 (a),s(b));let h=d+2.;let(p,t)=(1./(c*h
*e+f*g+5.),c*h*g-f     *e);let(q,x,y)=((
8.*((f*e-c*d*g)*         m-c*d*e-f*g-l*d
*n))as usize,(            40.+30.*p*(l*h
*m-t*n))as usize        ,(12.+15.*p*(l*h
 *n+t*m))as usize       );let o=x+80*y;
 if 0<y&&y<22&&0<x&&x<80&&z[o]<p{z[o]=p
 ;r[o]=b".,-~:;=!*#$@"[q]as char;}i+=
  0.02}j+=0.07}print!("\x1b[H");for k 
   in 0..1760{print!("{}",['\n',r[k]]
     [1.min(k%80)])}a+=0.04;b+=0.02
       ;/*std::thread::sleep(std::
         time::Duration::new(0,
              30000000))*/}}

fn sin_cos(f: f32) -> (f32, f32) {
    (sinf(f), cosf(f))
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
        sym main
    );
}
