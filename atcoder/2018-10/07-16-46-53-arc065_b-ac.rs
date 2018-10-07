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

struct G {
    N: usize,
    A: Vec<Vec<usize>>,
    P: Vec<usize>,
}

impl G {
    fn decomp(&mut self) {
        for v in 0..self.N {
            self.dfs(v, v);
        }
    }

    fn dfs(&mut self, v: usize, p: usize) {
        debug!((v, p, self.P[v]));
        if self.P[v] != self.N {
            return;
        }

        self.P[v] = p;

        for i in 0..self.A[v].len() {
            let w = self.A[v][i];
            self.dfs(w, p);
        }
    }
}

fn main() {
    let (N, K, L) = read!(usize, usize, usize);

    // subway instead of rails
    let mut R = vec![vec![]; N];
    let mut S = vec![vec![]; N];

    for _ in 0..K {
        let (u, v) = read!(usize, usize);
        R[u - 1].push(v - 1);
        R[v - 1].push(u - 1);
    }

    for _ in 0..L {
        let (u, v) = read!(usize, usize);
        S[u - 1].push(v - 1);
        S[v - 1].push(u - 1);
    }

    let mut rg = G {
        N: N,
        A: R,
        P: vec![N; N],
    };
    rg.decomp();

    let mut sg = G {
        N: N,
        A: S,
        P: vec![N; N],
    };
    sg.decomp();

    let mut m = BTreeMap::new();
    for v in 0..N {
        *m.entry((rg.P[v], sg.P[v])).or_insert(0) += 1;
    }

    let mut ns = vec![];
    for v in 0..N {
        ns.push(m[&(rg.P[v], sg.P[v])]);
    }

    debug!(sg.A, rg.A, m);

    println!("{}", ns.into_iter().map(|n| n.to_string()).vec().join(" "))
}
