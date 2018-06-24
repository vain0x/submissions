#![allow(unused_imports)]
#![allow(non_snake_case)]

use std::cmp::{max, min, Ordering};
use std::collections::*;
use std::fmt::{Debug, Display, Formatter};
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

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Rev<T>(pub T);

impl<T: PartialOrd> PartialOrd for Rev<T> {
    fn partial_cmp(&self, other: &Rev<T>) -> Option<Ordering> {
        other.0.partial_cmp(&self.0)
    }
}

impl<T: Ord> Ord for Rev<T> {
    fn cmp(&self, other: &Rev<T>) -> Ordering {
        other.0.cmp(&self.0)
    }
}

/// Wraps a partial-ord value to impl Ord.
#[derive(PartialEq, PartialOrd, Clone, Copy, Debug)]
pub struct OrdAdapter<T>(pub T);

impl<T: PartialEq> Eq for OrdAdapter<T> {}

impl<T: PartialOrd> Ord for OrdAdapter<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

fn S(N: i64) -> i64 {
    if N < 10 {
        N
    } else {
        N % 10 + S(N / 10)
    }
}

fn R(N: i64) -> f64 {
    N as f64 / S(N) as f64
}

fn f(N: i64) -> i64 {
    let mut n = N;
    let mut m = n;
    let mut d = 0;
    let mut b = 1;
    let mut xs = vec![n];

    while m > 0 && d < 15 {
        n += (9 - m % 10) * b;
        m /= 10;
        b *= 10;
        d += 1;

        xs.push(n);
    }

    debug!(xs);

    xs.into_iter()
        .min_by_key(|&n| (OrdAdapter(R(n)), n))
        .unwrap()
}

pub fn main() {
    // let mut v = Vec::new();
    // let mut n = 1;
    // for _ in 0..1000 {
    //     if n >= 1_000_000_000_000_000_i64 {
    //         break;
    //     }
    //     v.push(n);
    //     n = f(n + 1);
    // }
    // debug!(v, v.len());

    let K = read!(usize);

    let mut n = 1;
    for _ in 0..K {
        println!("{}", n);
        n = f(n + 1);
    }
}
