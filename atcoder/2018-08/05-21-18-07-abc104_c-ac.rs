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
            let entries = [$(&stringify!([$arg]:), &$arg as &Debug),*];
            stderr().write_fmt(format_args!("{:#?}\n", entries)).unwrap();
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

pub fn main() {
    let (D, G) = read!(usize, usize);
    let P = read![usize, usize; D];

    let mut mi = std::usize::MAX;
    for s in 0..(1 << D) {
        let mut count = 0;
        let mut score = 0;

        for i in 0..D {
            if s & (1 << i) != 0 {
                count += P[i].0;
                score += (i + 1) * 100 * P[i].0 + P[i].1;
            }
        }

        for i in (0..D).rev() {
            if s & (1 << i) != 0 || P[i].0 <= 1 {
                continue;
            }

            for _ in 0..P[i].0 - 1 {
                if score >= G {
                    break;
                }

                count += 1;
                score += (i + 1) * 100;
            }
        }

        if score >= G {
            mi = min(mi, count);
        }
    }

    println!("{}", mi);
    return;
}
