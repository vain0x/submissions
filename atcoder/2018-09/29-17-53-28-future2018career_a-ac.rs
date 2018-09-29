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

#[derive(Debug, Clone, Copy)]
struct Op {
    i: usize,
    j: usize,
    k: usize,
    l: usize,
    v: i32,
}

struct Solver {
    N: usize,
    M: usize,
    K: usize,
    A: Vec<i32>,

    // q[g][m] = heap (max_v, ai)
    // g = 0 : 要素の値を減らす対象の区間
    // g = 1 : 増やす対象
    // m: 区間長
    // max_v: 区間に対する変動幅の最大値
    q: Vec<Vec<BinaryHeap<(i32, usize)>>>,
}

fn g(v: i32) -> usize {
    if v >= 0 {
        0
    } else {
        1
    }
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

    // O(M)
    fn max_v(&self, l: usize, r: usize) -> i32 {
        let s = self.d(l).signum();
        if s == 0 {
            return 0;
        }

        // 符号が異なる要素は同じ区間に入れない。
        for i in l..r {
            if self.d(i).signum() != s {
                return 0;
            }
        }

        s * (l..r).map(|i| self.d(i).abs()).min().unwrap()
    }

    // O(M^3 log N)
    fn up(&mut self, i: usize) {
        for l in (i - min(i, self.M - 1))..i + 1 {
            for r in max(l, i + 1)..min(self.N, i + self.M) + 1 {
                let m = r - l;
                if m > self.M {
                    continue;
                }

                let max_v = self.max_v(l, r);
                self.q[g(max_v)][r - l].push((max_v.abs(), l));
            }
        }
    }

    fn op(&mut self, op: Op) {
        println!("{} {} {} {} {}", op.i + 1, op.j, op.k + 1, op.l, op.v);

        assert!(op.i < op.j && op.k < op.l && (op.j <= op.k || op.l <= op.i) && op.v > 0);

        for i in op.i..op.j {
            self.A[i] -= op.v;
            self.up(i);
        }

        for i in op.k..op.l {
            self.A[i] += op.v;
            self.up(i);
        }

        self.K -= 1;

        debug!((&self.A, self.score()));
    }

    fn peek(&mut self, g: usize, m: usize) -> Option<(i32, usize)> {
        while let Some(&(v, i)) = self.q[g][m].peek() {
            let u = self.max_v(i, i + m);
            debug!(("peek", g, m, v, i, u));
            if g == ::g(u) && v == u.abs() {
                return Some((v, i));
            }
            self.q[g][m].pop();
        }

        None
    }

    fn eval(&mut self, m: usize) -> Option<(Op, i32)> {
        self.peek(0, m).and_then(|(v0, i)| {
            self.peek(1, m).map(|(v1, k)| {
                let v = min(v0, v1);
                let gain = 2 * m as i32 * v;

                let op = Op {
                    i: i,
                    j: i + 1,
                    k: k,
                    l: k + 1,
                    v: v,
                };
                (op, gain)
            })
        })
    }

    fn go(mut self) {
        debug!((&self.A, self.score()));

        for i in 0..self.N {
            self.up(i);
        }

        while self.K >= 1 {
            let mut ma_op = None;
            let mut ma_gain = 0;

            for m in 1..self.M + 1 {
                if let Some((op, gain)) = self.eval(m) {
                    debug!(("eval", m, gain));
                    if ma_gain < gain {
                        ma_gain = gain;
                        ma_op = Some(op);
                    }
                }
            }

            if let Some(op) = ma_op {
                self.op(op);
            } else {
                break;
            }
        }
    }
}

fn main() {
    let (N, K) = read!(usize, usize);
    let A = read![i32; N];

    // 区間長の最大値
    let M = 13;

    let solver = Solver {
        N: N,
        M: M,
        K: K,
        A: A,
        q: vec![vec![BinaryHeap::new(); M + 1]; 2],
    };
    solver.go();
}
