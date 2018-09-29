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

fn main() {
    let N = read!(usize);
    let V = read![[i32]];

    let mut kv = vec![];

    let mut s = 0;

    for p in 0..2 {
        let mut m = BTreeMap::new();

        for i in 0..N {
            if i % 2 != p {
                continue;
            }

            *m.entry(V[i]).or_insert(0) += 1;
        }

        let mut v = m.into_iter().map(|(k, v)| (v, k)).vec();

        v.push((0, 1_000_000));
        v.push((0, 1_000_001));
        v.sort();
        v.reverse();
        kv.push(v);
    }

    let mut i = 0;
    let mut j = 0;
    while kv[0][i].1 == kv[1][j].1 {
        if kv[0][i + 1].0 < kv[1][j].0 {
            i += 1;
        } else {
            j += 1;
        }
    }

    s += N - (kv[0][i].0 + kv[1][j].0);

    println!("{}", s)
}
