#![allow(unused_imports)]
#![allow(non_snake_case)]

use std::cell::RefCell;
use std::cmp::{max, min, Ordering};
use std::collections::*;
use std::fmt::{Debug, Formatter};
use std::io::*;
use std::mem::{replace, swap};
use std::ops::*;
use std::rc::Rc;

// -----------------------------------------------
// Framework
// -----------------------------------------------

#[allow(unused_macros)]
macro_rules! read {
    ([$t:ty] ; $n:expr) =>
        ((0..$n).map(|_| read!([$t])).collect::<Vec<_>>());
    ($($t:ty),+ ; $n:expr) =>
        ((0..$n).map(|_| read!($($t),+)).collect::<Vec<_>>());
    ([$t:ty]) =>
        (rl().split_whitespace().map(|w| w.parse().unwrap()).collect::<Vec<$t>>());
    ($t:ty) =>
        (rl().parse::<$t>().unwrap());
    ($($t:ty),*) => {{
        let buf = rl();
        let mut w = buf.split_whitespace();
        ($(w.next().unwrap().parse::<$t>().unwrap()),*)
    }};
}

#[allow(unused_macros)]
macro_rules! debug {
    ($($arg:expr),*) => {
        #[cfg(debug_assertions)]
        {
            let entries = [$(&stringify!($arg),  &$arg as &Debug),*];
            stderr().write_fmt(format_args!("{:?}\n", entries)).unwrap();
        }
    };
}

#[allow(dead_code)]
fn rl() -> String {
    let mut buf = String::new();
    stdin().read_line(&mut buf).unwrap();
    buf.trim_right().to_owned()
}

trait IteratorExt: Iterator + Sized {
    fn vec(self) -> Vec<Self::Item> {
        self.collect()
    }
}

impl<T: Iterator> IteratorExt for T {}

// -----------------------------------------------
// Solution
// -----------------------------------------------

// 位置 0 にマーブルが n 個あって、それらを範囲 [gl, gr) の箱に移すときにかかる最小の操作回数
fn calc(n: i32, gl: i32, gr: i32) -> i32 {
    let lr = min(0, gr);
    let rl = max(0, gl);
    let rr = max(0, min(gr, max(gl + n, (n + 1) / 2)));
    let rw = rr - rl;
    let rn = rl * rw + rw * (rw - 1) / 2;
    let lw = n - rw;
    let ll = lr - lw;
    let ln = (1 - lr) * lw + lw * (lw - 1) / 2;
    // debug!(n, gl, gr);
    // debug!(ll, lr, rl, rr);
    // debug!(lw, rw, ln, rn);
    ln + rn
}

pub fn main() {
    let (R, G, B) = read!(i32, i32, i32);

    let mut mi = std::i32::MAX;
    // let (gl, gr) = (-50, 50); { {
    for gl in -500..500 {
        for gr in gl + G..500 {
            // (-inf, l): all red
            // [l, r): all green
            // [r, inf): all blue

            let rn = calc(R, -10000, gl + 100);
            let gn = calc(G, gl, gr);
            let bn = calc(B, gr - 100, 10000);
            mi = min(mi, rn + gn + bn);
        }
    }

    println!("{}", mi);
    return;
}
