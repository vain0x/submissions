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
use std::fmt::{Debug, Display, Formatter, Write as FmtWrite};
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

const P: i64 = 1_000_000_007;

struct Solver {
    N: usize,
    G: Vec<Vec<usize>>,
    done: Vec<bool>,
    dpw: Vec<i64>,
    dpb: Vec<i64>,
}

impl Solver {
    fn calc(&mut self, u: usize) {
        self.done[u] = true;

        let mut w = 1;
        let mut b = 1;

        for i in 0..self.G[u].len() {
            let v = self.G[u][i];

            if self.done[v] {
                continue;
            }

            self.calc(v);

            w *= (self.dpw[v] + self.dpb[v]) % P;
            w %= P;
            b *= self.dpw[v];
            b %= P;
        }

        self.dpw[u] = w;
        self.dpb[u] = b;
    }
}

fn main() {
    std::thread::Builder::new()
        .stack_size(16 * 1024 * 1024)
        .spawn(|| {
            let N = read!(usize);
            let T = read![usize, usize; N - 1];

            let mut G = vec![vec![]; N];
            for (u, v) in T {
                G[u - 1].push(v - 1);
                G[v - 1].push(u - 1);
            }

            let mut solver = Solver {
                N: N,
                G: G,
                done: vec![false; N],
                dpw: vec![0; N],
                dpb: vec![0; N],
            };
            solver.calc(0);

            println!("{}", (solver.dpw[0] + solver.dpb[0]) % P);
        })
        .unwrap()
        .join()
        .unwrap();
}
