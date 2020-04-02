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

const P: i64 = 1_000_000_007;

pub fn pow(x: i64, n: i64) -> i64 {
    let (mut x, mut y, mut n) = (x % P, 1_i64, n);
    while n > 0 {
        if n % 2 != 0 {
            y = (y * x) % P;
            n -= 1;
        }

        x = (x * x) % P;
        n /= 2;
    }
    y
}

#[derive(Clone, Copy)]
struct X {
    /// 場合の数
    combo: i64,
    /// 部分木のサイズ (頂点数)
    size: usize,
}

impl X {
    fn empty() -> X {
        X { combo: 1, size: 0 }
    }

    fn append(self, other: X) -> X {
        X {
            combo: self.combo * other.combo % P,
            size: self.size + other.size,
        }
    }
}

struct Solver {
    N: usize,
    G: Vec<Vec<usize>>,
    rev: Vec<HashMap<usize, usize>>,
    dp: Vec<Vec<X>>,
    l_dp: Vec<Vec<X>>,
    r_dp: Vec<Vec<X>>,
    fact: Vec<i64>,
    fact_inv: Vec<i64>,
}

impl Solver {
    fn new(G: Vec<Vec<usize>>) -> Self {
        let N = G.len();

        // rev[u][&v] = i <=> G[u][i] = v
        let rev = (0..N)
            .map(|u| (0..G[u].len()).map(|i| (G[u][i], i)).collect())
            .collect();

        let dp = (0..N)
            .map(|u| vec![X::empty(); G[u].len() + 1])
            .collect::<Vec<_>>();
        let l_dp = dp.clone();
        let r_dp = dp.clone();

        let M = N + 1;
        let mut fact = vec![0; M];
        fact[0] = 1;
        for i in 1..M {
            fact[i] = fact[i - 1] * (i as i64) % P;
        }

        let mut fact_inv = vec![0; M];
        fact_inv[M - 1] = pow(fact[M - 1], P - 2); // フェルマーの小定理
        for i in (0..M - 1).rev() {
            fact_inv[i] = fact_inv[i + 1] * ((i + 1) as i64) % P;
        }

        Solver {
            N: N,
            G: G,
            rev: rev,
            dp: dp,
            l_dp: l_dp,
            r_dp: r_dp,
            fact: fact,
            fact_inv: fact_inv,
        }
    }

    /// u の i 番目の子ノードを v として、{ combo: dp(v) / size(v)!, size: size(v) } を計算する。
    fn factor(&self, u: usize, i: usize) -> X {
        let x = self.dp[u][i];
        X {
            combo: x.combo * self.fact_inv[x.size] % P,
            size: x.size,
        }
    }

    /// u の各子ノード v に対する dp(v) / size(v)! の積を x.combo とし、
    /// 各子ノードのサイズの総和を x.size とするとき、
    /// dp(u), size(u) の値を計算する。
    fn compute(&self, _u: usize, x: X) -> X {
        X {
            combo: self.fact[x.size] * x.combo % P,
            size: x.size + 1,
        }
    }

    fn dfs1(&mut self, u: usize, p: Option<usize>) -> X {
        let mut x = X::empty();

        for i in 0..self.G[u].len() {
            let v = self.G[u][i];
            if Some(v) == p {
                continue;
            }

            let vx = self.dfs1(v, Some(u));
            self.dp[u][i] = vx;

            x = x.append(self.factor(u, i));
        }

        self.compute(u, x)
    }

    fn dfs2(&mut self, u: usize, p: Option<usize>, px: X) {
        if let Some(p) = p {
            let i = self.rev[u][&p];
            self.dp[u][i] = px;
        }

        let d = self.G[u].len();

        for i in 0..d {
            self.l_dp[u][i + 1] = self.l_dp[u][i].append(self.factor(u, i));
        }

        for i in (0..d).rev() {
            self.r_dp[u][i] = self.r_dp[u][i + 1].append(self.factor(u, i));
        }

        for i in 0..d {
            let v = self.G[u][i];
            if Some(v) == p {
                continue;
            }

            let ux = self.compute(u, self.l_dp[u][i].append(self.r_dp[u][i + 1]));
            self.dfs2(v, Some(u), ux);
        }
    }

    fn solve(&mut self) {
        self.dfs1(0, None);
        self.dfs2(0, None, X::empty());

        for u in 0..self.N {
            println!("{}", self.compute(u, self.r_dp[u][0]).combo);
        }
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
