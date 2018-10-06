#![allow(unused_imports)]
#![allow(non_snake_case)]

use std::cell::RefCell;
use std::cmp::{max, min, Ordering};
use std::collections::*;
use std::fmt::{Debug, Formatter, Write as FmtWrite};
use std::io::{stderr, stdin, BufRead, Write};
use std::mem::{replace, swap};
use std::ops::*;
use std::rc::Rc;

// -----------------------------------------------
// Framework <https://github.com/vain0x/procon>
// -----------------------------------------------

#[cfg(debug_assertions)]
include!{"./procon/debug.rs"}

#[cfg(not(debug_assertions))]
macro_rules! debug {
    ($($arg:expr),*) => {};
}

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

pub fn factorize(mut x: i64) -> Vec<i64> {
    let mut qs = vec![];
    let r = (x as f64).sqrt() as i64 + 1;

    for p in 2..r as i64 {
        let mut q = 1;

        while x >= p && x % p == 0 {
            x /= p;
            q *= p;
        }

        if q > 1 {
            qs.push(p);
        }
    }

    if x > 1 {
        qs.push(x);
    }

    qs
}

fn main() {
    let (N, M) = read!(i64, i64);
    let x = M / N;
    let g = if M % N == 0 {
        x
    } else {
        let ps = factorize(M);
        debug!(ps);

        let mut queue = BinaryHeap::new();
        let mut done = BTreeSet::new();
        let mut ma = 1;

        queue.push(M);
        while let Some(d) = queue.pop() {
            debug!(d);
            let k = M / d;
            if k >= N {
                ma = max(ma, d);
            }

            for &p in &ps {
                let x = d / p;
                if d % p == 0 && d >= p && done.insert(x) {
                    queue.push(x);
                }
            }
        }

        ma
    };
    println!("{}", g)
}
