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

/// Wraps a partial-ord value to impl Ord.
#[derive(PartialEq, PartialOrd, Clone, Copy, Debug)]
pub struct OrdAdapter<T>(pub T);

impl<T: PartialEq> Eq for OrdAdapter<T> {}

impl<T: PartialOrd> Ord for OrdAdapter<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

// -----------------------------------------------
// Solution
// -----------------------------------------------

fn main() {
    let (_, M) = read!(usize, usize);
    let Q = read![i64, f64, f64; M];

    let mut P = Q.iter().map(|&(p, _, _)| p).vec();
    P.sort();
    P.dedup();
    let mut PInv = BTreeMap::new();
    for i in 0..P.len() {
        PInv.insert(P[i], i);
    }
    let N = P.len();
    let Q = Q.into_iter().map(|(p, a, b)| (PInv[&p], a, b)).vec();

    let mut boxes = vec![(1.0, 0.0); N];

    let bucket_size = 100;

    let mut buckets = vec![];
    {
        let mut l = 0;
        let mut r = 1;
        while r <= N {
            if r == N || (r % bucket_size == 0) {
                let stat = (l, r, 1.0, 0.0);
                buckets.push(stat);
                l = r;
                r = l + 1;
                continue;
            }
            r += 1;
        }
    }

    debug!(buckets);

    let mut mi = 1.0;
    let mut ma = 1.0;
    for qi in 0..M {
        let (pi, pa, pb) = Q[qi];
        boxes[pi] = (pa, pb);

        let mut v = 1.0;
        for bi in 0..buckets.len() {
            let (bl, br, _, _) = buckets[bi];
            if bl <= pi && pi < br {
                let mut xa = 1.0;
                let mut xb = 0.0;
                for i in bl..br {
                    let (a, b) = boxes[i];
                    xa *= a;
                    xb = a * xb + b;
                }
                buckets[bi] = (bl, br, xa, xb);
            }

            let (_, _, ba, bb) = buckets[bi];
            v = ba * v + bb;
        }

        debug!((qi, pi, pa, pb), buckets, v);

        mi = min(OrdAdapter(mi), OrdAdapter(v)).0;
        ma = max(OrdAdapter(ma), OrdAdapter(v)).0;
    }

    println!("{}", mi);
    println!("{}", ma);
}
