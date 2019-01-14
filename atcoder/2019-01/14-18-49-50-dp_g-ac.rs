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
    // len[v]: 点 v から出る最長のパスの長さ
    len: Vec<usize>,
    // next[v]: 点 v から出る最長のパスの次の点
    next: Vec<usize>,
    N: usize,
    G: Vec<Vec<usize>>,
}

impl Solver {
    fn dfs(&mut self, u: usize) -> usize {
        let mut max_len = 0;

        if self.len[u] != std::usize::MAX {
            return self.len[u];
        }

        for i in 0..self.G[u].len() {
            let v = self.G[u][i];

            let len = self.dfs(v) + 1;
            if max_len < len {
                max_len = len;
                self.next[u] = v;
            }
        }

        self.len[u] = max_len;
        max_len
    }

    fn run(mut self) {
        for v in 0..self.N {
            self.dfs(v);
        }

        let m = (0..self.N).map(|v| self.len[v]).max().unwrap();
        println!("{}", m);
    }
}

fn main() {
    let (N, M) = read!(usize, usize);
    let E = read![usize, usize; M];

    let mut G = vec![vec![]; N];
    for (u, v) in E {
        G[u - 1].push(v - 1);
    }

    Solver {
        N: N,
        G: G,
        len: vec![std::usize::MAX; N],
        next: vec![0; N],
    }
    .run()
}
