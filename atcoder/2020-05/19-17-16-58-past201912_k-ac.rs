//! Framework <https://github.com/vain0x/procon>

#![allow(non_snake_case)]
#![allow(unused_imports)]

use std::cmp::{max, min, Ordering};
use std::collections::*;
use std::fmt::{Debug, Display, Formatter, Write as FmtWrite};
use std::io::{stderr, stdin, BufRead, Write};

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

    #[allow(deprecated)]
    buf.trim_right().to_owned()
}

// -----------------------------------------------
// Solution
// -----------------------------------------------

struct Solve {
    N: usize,
    g: Vec<Vec<usize>>,
    root: usize,
    ancestors: Vec<Vec<usize>>,
    depth: Vec<usize>,
}

impl Solve {
    fn solve(&mut self) {
        let N = self.N;
        let root = self.root;

        self.dfs(root, N);

        let Q = read!(usize);
        for _ in 0..Q {
            let (mut a, mut b) = read!(usize, usize);
            a -= 1;
            b -= 1;

            let mut ad = self.depth[a];
            let bd = self.depth[b];
            while ad > bd {
                let mut h = 0;
                while ad - bd >= 1 << (h + 1) {
                    h += 1;
                }

                a = self.ancestors[a][h];
                ad = self.depth[a];
            }

            if a == b {
                println!("Yes");
            } else {
                println!("No");
            }
        }
    }

    fn dfs(&mut self, u: usize, p: usize) {
        let N = self.N;

        for i in 0..self.g[u].len() {
            let v = self.g[u][i];
            if v == p {
                continue;
            }

            let mut a = u;
            let mut h = 0;
            while a != N {
                self.ancestors[v].push(a);
                a = self.ancestors[a].get(h).cloned().unwrap_or(N);
                h += 1;
            }

            self.depth[v] = self.depth[u] + 1;

            self.dfs(v, u);
        }
    }
}

fn main() {
    let N = read!(usize);

    let mut g = vec![vec![]; N];
    let mut root = N;
    for i in 0..N {
        let u = read!(isize);
        if u < 0 {
            root = i;
        } else {
            let u = u as usize - 1;
            g[u].push(i);
        }
    }

    Solve {
        N: N,
        g: g,
        root: root,
        ancestors: vec![vec![]; N],
        depth: vec![0; N],
    }
    .solve()
}
