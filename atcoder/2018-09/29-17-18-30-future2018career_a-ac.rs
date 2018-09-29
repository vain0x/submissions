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
    K: usize,
    A: Vec<i32>,

    // q[g][m] = heap (d, ai)
    // g = 0 : 要素の値を減らす対象の区間
    // g = 1 : 増やす対象
    // m: 区間長
    // d: 区間に一様加算できる値の最大値
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

    fn d2(&self, i: usize) -> i32 {
        let d1 = self.d(i);
        let d2 = self.d(i + 1);

        // 符号が違ったら同じ区間には入れない
        if (d1 + d2).abs() != d1.abs() + d2.abs() {
            return 0;
        }

        d1.signum() * min(d1.abs(), d2.abs())
    }

    fn up(&mut self, i: usize) {
        let d = self.d(i);
        self.q[g(d)][1].push((d.abs(), i));

        if i + 1 < self.N {
            let d = self.d2(i);
            self.q[g(d)][2].push((d.abs(), i));
        }
    }

    fn op(&mut self, op: Op) {
        println!(
            "{} {} {} {} {}",
            1 + op.i,
            1 + op.j,
            1 + op.k,
            1 + op.l,
            op.v
        );

        assert!(op.i <= op.j && op.k <= op.l && (op.j < op.k || op.l < op.i) && op.v > 0);

        for i in op.i..op.j + 1 {
            self.A[i] -= op.v;
            self.up(i);
        }

        for i in op.k..op.l + 1 {
            self.A[i] += op.v;
            self.up(i);
        }

        self.K -= 1;

        debug!((&self.A, self.score()));
    }

    fn eval1(&mut self) -> Option<(Op, i32)> {
        let (i, d0) = match self.q[0][1].peek() {
            None => return None,
            Some(&(d, di)) => {
                debug!(("eval1 g=0", d, di, self.d(di)));
                if d != self.d(di) {
                    self.q[0][1].pop();
                    return self.eval1();
                }
                (di, d)
            }
        };

        let (k, d1) = match self.q[1][1].peek() {
            None => return None,
            Some(&(d, di)) => {
                debug!(("eval1 g=1", d, di, self.d(di)));
                if d != -self.d(di) {
                    self.q[1][1].pop();
                    return self.eval1();
                }
                (di, d)
            }
        };

        let v = min(d0, d1);
        let gain = 2 * v;
        let op = Op {
            i: i,
            j: i,
            k: k,
            l: k,
            v: v,
        };
        Some((op, gain))
    }

    fn eval2(&mut self) -> Option<(Op, i32)> {
        let (d0, i) = match self.q[0][2].peek() {
            None => return None,
            Some(&(d, di)) => {
                if d != self.d2(di) {
                    self.q[0][2].pop();
                    return self.eval2();
                }
                (d, di)
            }
        };

        let (d1, k) = match self.q[1][2].peek() {
            None => return None,
            Some(&(d, di)) => {
                if d != -self.d2(di) {
                    self.q[1][2].pop();
                    return self.eval2();
                }
                (d, di)
            }
        };

        let v = min(d0, d1);
        let gain = 4 * v;

        let op = Op {
            i: i,
            j: i + 1,
            k: k,
            l: k + 1,
            v: v,
        };
        Some((op, gain))
    }

    fn go(mut self) {
        debug!((&self.A, self.score()));

        for i in 0..self.N {
            self.up(i);
        }

        while self.K >= 1 {
            let mut op = None;
            let mut gain = 0;

            if let Some((op1, gain1)) = self.eval1() {
                if gain1 > gain {
                    op = Some(op1);
                    gain = gain1;
                }
            }

            if let Some((op2, gain2)) = self.eval2() {
                if gain2 > gain {
                    op = Some(op2);
                    gain = gain2;
                }
            }

            if let Some(op) = op {
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

    let solver = Solver {
        N: N,
        K: K,
        A: A,
        q: vec![vec![BinaryHeap::new(); 3]; 2],
    };
    solver.go();
}
