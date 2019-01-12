// -----------------------------------------------
// Framework <https://github.com/vain0x/procon>
//
// See the bottom of file for solution.
// -----------------------------------------------

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

#[allow(unused_macros)]
macro_rules! debug {
    ($($e:expr),*) => {
        #[cfg(debug_assertions)]
        $({
            let (e, mut err) = (stringify!($e), stderr());
            writeln!(err, "\x1B[33m{}\x1B[0m = {:?}", e, $e).unwrap()
        })*
    };
}

#[allow(unused_macros)]
macro_rules! read {
    ([$t:ty] ; $n:expr) =>
        ((0..$n).map(|_| read!([$t])).collect::<Vec<_>>());
    ($($t:ty),+ ; $n:expr) =>
        ((0..$n).map(|_| read!($($t),+)).collect::<Vec<_>>());
    ([$t:ty]) =>
        (rl().split_whitespace().map(|w| w.parse().unwrap()).collect::<Vec<$t>>());
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

// -----------------------------------------------
// Solution
// -----------------------------------------------

struct Solver {
    l: usize,
    r: usize,
    t: usize,
    x: i64,
    q: usize,
    N: usize,
    A: Vec<i64>,
    Q: usize,
    X: Vec<i64>,
    sums: Vec<i64>,
    even_sums: Vec<i64>,
}

impl Solver {
    fn d(&self, i: usize) -> i64 {
        (self.A[i] - self.x).abs()
    }

    fn shift(&mut self) {
        while self.l >= 1 && self.A[self.l - 1] >= self.x {
            self.l -= 1;
            self.r -= 1;
        }

        while self.l >= 1 && self.l < self.r && self.d(self.l - 1) <= self.d(self.r - 1) {
            self.l -= 1;
            self.r -= 1;
        }

        loop {
            if self.r + self.t >= self.N {
                break;
            }

            if self.takahashi_turn() {
                self.t += 1
            } else {
                // aoki turn
                if self.l == 0 || self.d(self.l - 1) > self.d(self.r) {
                    self.r += 1
                } else {
                    self.l -= 1
                }
            }
        }
    }

    fn takahashi_turn(&self) -> bool {
        self.r - self.l >= self.t
    }

    fn query(&self) -> i64 {
        let mut s = 0_i64;

        s += self.sums[self.N] - self.sums[self.N - self.t];

        let l = self.l;
        let s2;
        let nt = self.takahashi_turn();

        if (nt && l % 2 == 1) || (!nt && l % 2 == 0) {
            s2 = self.even_sums[l];
        } else {
            s2 = self.sums[l] - self.even_sums[l];
        }
        s += s2;

        {
            let tl = self.N - self.t;
            let tls = self.sums[self.N] - self.sums[self.N - self.t];
            debug!((nt, tl, tls, s2));
        }

        s
    }

    fn solve(mut self) {
        let mut X = self
            .X
            .iter()
            .enumerate()
            .map(|(q, &x)| (x, q))
            .collect::<Vec<_>>();
        X.sort();
        X.reverse();
        let mut solutions = vec![0; self.Q];

        for (x, q) in X {
            self.x = x;
            self.q = q;
            self.shift();
            solutions[q] = self.query();

            let (x, q, l, r, t, s) = (self.x, self.q, self.l, self.r, self.t, self.query());
            debug!((q, x, l, r, t, s));
        }

        for s in solutions {
            println!("{}", s)
        }
    }
}

fn main() {
    let (N, Q) = read!(usize, usize);
    let A = read![[i64]];
    let X = read![i64; Q];

    let mut sums = vec![0; N + 1];
    let mut even_sums = vec![0; N + 1];

    for i in 0..N {
        sums[i + 1] += sums[i] + A[i];
        even_sums[i + 1] += even_sums[i] + if i % 2 == 0 { A[i] } else { 0 };
    }

    debug!(sums, even_sums);

    Solver {
        A: A,
        X: X,
        N: N,
        Q: Q,
        l: N,
        r: N,
        t: 0,
        x: 0,
        q: 0,
        sums: sums,
        even_sums: even_sums,
    }
    .solve()
}
