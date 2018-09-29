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
    A: Vec<i32>,
    dmaxq: BinaryHeap<(i32, i32, usize)>,
    dminq: BinaryHeap<Rev<(i32, i32, usize)>>,
}

impl Solver {
    fn score(&self) -> i64 {
        let mut s = 0_i64;
        for i in 0..self.A.len() {
            s += self.d(i).abs() as i64;
        }
        s
    }

    fn d(&self, i: usize) -> i32 {
        let x = (1 + i) as i32;
        self.A[i] - x
    }

    fn up(&mut self, i: usize) {
        let d = self.d(i);
        if d > 0 {
            self.dmaxq.push((d, self.A[i], i));
        }
        if d < 0 {
            self.dminq.push(Rev((d, self.A[i], i)));
        }
    }

    fn op(&mut self, i: usize, k: usize, v: i32) {
        assert!(
            i != k && v > 0 && self.A[i] > v && self.A[k] + v <= self.N as i32,
            "i={} k={} v={}",
            i,
            k,
            v
        );

        println!("{} {} {} {} {}", 1 + i, 1 + i, 1 + k, 1 + k, v);

        self.A[i] -= v;
        self.A[k] += v;

        self.up(i);
        self.up(k);

        self.K -= 1;

        debug!((&self.A, self.score()));
    }

    fn go(mut self) {
        debug!((&self.A, self.score()));

        for i in 0..self.N {
            self.up(i);
        }

        while self.K >= 1 {
            let dmaxi = match self.dmaxq.peek() {
                None => break,
                Some(&(_, da, di)) => {
                    if da != self.A[di] {
                        self.dmaxq.pop();
                        continue;
                    }
                    di
                }
            };

            let dmini = match self.dminq.pop() {
                None => break,
                Some(Rev((_, da, di))) => {
                    if da != self.A[di] {
                        continue;
                    }
                    di
                }
            };

            let v = min(self.d(dmaxi), self.d(dmini).abs());
            self.op(dmaxi, dmini, v);
        }
    }
}

fn main() {
    let (N, K) = read!(usize, usize);
    let A = read![i32; N];

    let solver = Solver {
        N: N,
        K: K,
        A: A,
        dmaxq: BinaryHeap::new(),
        dminq: BinaryHeap::new(),
    };
    solver.go();
}
