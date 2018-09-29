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

// -----------------------------------------------
// Solution
// -----------------------------------------------

struct Solver {
    N: usize,
    K: usize,
    A: Vec<usize>,
    lq: BinaryHeap<Rev<(usize, usize)>>,
    rq: BinaryHeap<(usize, usize)>,
}

impl Solver {
    fn op(&mut self, i: usize, k: usize, v: usize) {
        assert!(i != k && self.A[i] > v && self.A[k] + v <= self.N);

        println!("{} {} {} {} {}", 1 + i, 1 + i, 1 + k, 1 + k, v);

        self.A[i] -= v;
        self.A[k] += v;

        self.lq.push(Rev((self.A[i], i)));
        self.rq.push((self.A[i], i));

        self.lq.push(Rev((self.A[k], k)));
        self.rq.push((self.A[k], k));

        self.K -= 1;

        debug!(self.A);
    }

    fn go(mut self) {
        // A[0..L] はソート済み
        let mut L = 0;

        for i in 0..self.N {
            self.lq.push(Rev((self.A[i], i)));
            self.rq.push((self.A[i], i));
        }

        while self.K >= 1 && L < self.N {
            if self.A[L] == 1 + L {
                L += 1;
                continue;
            }

            if self.A[L] > 1 + L {
                let Rev((la, li)) = self.lq.pop().unwrap();
                if !(L <= li && la == self.A[li]) {
                    continue;
                }

                let v = self.A[L] - (1 + L);
                self.op(L, li, v);
            } else {
                let (ra, ri) = self.rq.pop().unwrap();
                if !(L <= ri && ra == self.A[ri]) {
                    continue;
                }
                let v = (1 + L) - self.A[L];
                self.op(ri, L, v);
            }
        }
    }
}

fn main() {
    let (N, K) = read!(usize, usize);
    let A = read![usize; N];

    let solver = Solver {
        N: N,
        K: K,
        A: A,
        lq: BinaryHeap::new(),
        rq: BinaryHeap::new(),
    };
    solver.go();
}
