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

fn main() {
    let (_N, K, Q) = read!(usize, usize, usize);
    let A = read![[i64]];

    let mut mi = std::i64::MAX;

    let mut ys = A.to_owned();
    ys.sort();
    ys.dedup();

    for y in ys {
        // Y >= y になる操作だけ考える。つまり、y 未満の要素は取り除かないとする。
        // y 未満の要素を含む部分列には操作を適用できないので、そこで列が分離していると考える。
        // y 未満の要素を列から除去して、列をいくつかの部分列に分離する。
        // X - Y を最小化するには、それらの部分列から最小の要素を取り除いていくことで、X を最小化すればいい。

        let mut available = vec![];

        let mut chunks = vec![];
        let mut l = 0;
        for r in 0..A.len() + 1 {
            if r < A.len() && A[r] >= y {
                continue;
            }

            if l < r {
                chunks.push(&A[l..r]);
            }
            l = r + 1;
        }

        debug!(y, chunks);

        for chunk in chunks {
            let mut chunk = chunk.to_owned();
            chunk.sort();
            available.extend(chunk.into_iter().rev().skip(K - 1));
        }

        if available.len() < Q {
            continue;
        }

        available.sort();
        debug!(available);

        let y = available[0];
        let x = available[Q - 1];
        mi = min(mi, x - y);
    }

    println!("{}", mi)
}
