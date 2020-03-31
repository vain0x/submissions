//! ----------------------------------------------
//! Framework <https://github.com/vain0x/procon>
//!
//! See the bottom of file for solution.
//! ----------------------------------------------

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

/// Print values to standard error if debug mode.
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

/// Read from standard input and parse each word.
/// - `read!(T, U, ..)` parses a line as a tuple of words.
/// - `read![[T]]` parses a line as an array of words.
/// - `read![..; N]` parses `N` lines, using `read!(..)` repeatedly.
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

/// Read a line from standard input.
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

#[derive(Clone, Copy, Debug)]
struct A {
    inner: [usize; 2],
    len: usize,
}

impl A {
    fn m_empty() -> Self {
        A {
            inner: [0, 0],
            len: 0,
        }
    }

    fn m_append(self, other: Self) -> Self {
        let mut items = [self.inner[0], self.inner[1], other.inner[0], other.inner[1]];
        items.sort();

        A {
            inner: [items[3], items[2]],
            len: min(self.len + other.len, 2),
        }
    }

    fn add_assign(&mut self, x: usize) {
        if self.len == 0 {
            self.len = 1;
        }

        for i in 0..self.len {
            self.inner[i] += x;
        }
    }

    fn add(self, x: usize) -> Self {
        let mut t = self;
        t.add_assign(x);
        t
    }

    fn max(self) -> usize {
        self.inner[0]
    }
}

struct Solver {
    N: usize,
    G: Vec<Vec<usize>>,
    rev: Vec<HashMap<usize, usize>>,
    dp: Vec<Vec<A>>,
    l_dp: Vec<Vec<A>>,
    r_dp: Vec<Vec<A>>,
}

impl Solver {
    fn new(G: Vec<Vec<usize>>) -> Self {
        let N = G.len();

        // rev[u][&v] = i <=> G[u][i] = v
        let rev = (0..N)
            .map(|u| (0..G[u].len()).map(|i| (G[u][i], i)).collect())
            .collect();

        let dp = (0..N)
            .map(|u| vec![A::m_empty(); G[u].len() + 1])
            .collect::<Vec<_>>();
        let l_dp = dp.clone();
        let r_dp = dp.clone();

        Self {
            N: N,
            G: G,
            rev: rev,
            dp: dp,
            l_dp: l_dp,
            r_dp: r_dp,
        }
    }

    fn dfs1(&mut self, u: usize, p: Option<usize>) -> A {
        let mut ma = A::m_empty();

        for i in 0..self.G[u].len() {
            let v = self.G[u][i];
            if Some(v) == p {
                // dp[u][i] は dfs2 で設定する。
                continue;
            }

            let va = self.dfs1(v, Some(u));
            self.dp[u][i] = va;

            ma = ma.m_append(va.add(1));
        }

        ma
    }

    fn dfs2(&mut self, u: usize, p: Option<usize>, pa: A) {
        if let Some(p) = p {
            let i = self.rev[u][&p];
            self.dp[u][i] = pa;
        }

        let d = self.G[u].len();

        for i in 0..d {
            self.l_dp[u][i + 1] = self.l_dp[u][i].m_append(self.dp[u][i].add(1));
        }

        for i in (0..d).rev() {
            self.r_dp[u][i] = self.r_dp[u][i + 1].m_append(self.dp[u][i].add(1));
        }

        for i in 0..d {
            let v = self.G[u][i];
            if Some(v) == p {
                continue;
            }

            let ua = self.l_dp[u][i].m_append(self.r_dp[u][i + 1]);
            self.dfs2(v, Some(u), ua);
        }
    }

    fn solve(&mut self) {
        self.dfs1(0, None);
        self.dfs2(0, None, A::m_empty());

        let d = (0..self.N).map(|u| self.r_dp[u][0].max()).max().unwrap();
        debug!(d);

        let winner = if d % 3 != 1 { "First" } else { "Second" };
        println!("{}", winner);
    }
}

fn main() {
    let N = read!(usize);
    let mut G = vec![vec![]; N];

    for (mut u, mut v) in read![usize, usize; N - 1] {
        u -= 1;
        v -= 1;

        G[u].push(v);
        G[v].push(u);
    }

    Solver::new(G).solve()
}
