#![allow(unused_imports)]
#![allow(non_snake_case)]

use std::cmp::{max, min, Ordering};
use std::collections::*;
use std::fmt::{Debug, Formatter};
use std::io::*;
use std::ops::*;
use std::*;

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
            let entries = [$(&stringify!([$arg]:), &$arg as &Debug),*];
            stderr().write_fmt(format_args!("{:#?}\n", entries)).unwrap();
        }
    };
}

#[allow(dead_code)]
fn rl() -> String {
    let mut buf = String::new();
    io::stdin().read_line(&mut buf).unwrap();
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

fn p() {
    let n = 4;
    let m = 3;
    let d = 2;
    let mut H = vec![Vec::new(); m];
    let mut B = 0;

    for x in 0..n {
        for y in 0..n {
            for z in 0..n {
                let b = if (x as i32 - y).abs() == d { 1 } else { 0 }
                    + if (y - z).abs() == d { 1 } else { 0 };

                H[b].push(format!("{:?}", (x, y, z)));
                B += b;
            }
        }
    }

    let nm = (n as f64).powf(m as f64);
    debug!(H, B, nm);
    println!("{}", B as f64 / nm);
}

pub fn main() {
    let (n, m, d) = read!(i64, usize, i64);

    let p = if d == 0 {
        1_f64 / n as f64
    } else {
        // [n-d, d] -> ((n-d) - d)/n * (2/n)
        // <=d & <= n-d ->
        let k2 = (n - d) - min(n - d, d);
        let k1 = min(d, n - d) + (n - max(d, n - d));
        (k1 as f64 + 2_f64 * k2 as f64) / n as f64 / n as f64
    };
    let q = p * (m - 1) as f64;
    println!("{}", q);
    return;
}
