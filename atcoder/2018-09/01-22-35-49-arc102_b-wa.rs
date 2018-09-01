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
        $(writeln!(stderr(), "{} = {:?}", stringify!($arg), $arg).unwrap());*
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

struct InfiniteLoopDetector(i32);

impl InfiniteLoopDetector {
    fn check(&mut self) {
        #[cfg(debug_assertions)]
        {
            self.0 += 1;
            if self.0 >= 10_000 {
                panic!("infinite loop?")
            }
        }
    }
}

fn main() {
    let mut det = InfiniteLoopDetector(0);

    let L = read!(usize);

    let mut es = vec![];

    let mut h = 0;
    let mut p = 1;
    while p * 3 < L {
        p *= 3;
        h += 1;

        // det.check();
    }
    let N = h + 1;

    let mut q = p;
    for i in 1..N {
        q /= 3;
        es.push((i, i + 1, 0));
        es.push((i, i + 1, q));
        es.push((i, i + 1, q * 2));
    }

    // s 未満の経路は生成済み
    let mut s = p;

    let mut j = 1;

    while s < L {
        let t = L - s;
        while p > t {
            p /= 3;
            h -= 1;
            j += 1;

            // det.check();
        }

        es.push((1, j, s));
        s += p;

        // det.check();
    }

    println!("{} {}", N, es.len());
    for (u, v, w) in es {
        println!("{} {} {}", u, v, w);
    }
}
