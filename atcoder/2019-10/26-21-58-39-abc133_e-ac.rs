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

struct Solver {
    K: i64,
    G: Vec<Vec<usize>>,
    fact: Vec<i64>,
    fact_inv: Vec<i64>,
}

impl Solver {
    fn perm(&self, n: i64, r: i64) -> i64 {
        if n < 0 || n < r {
            return 0;
        }

        self.fact[n as usize] * self.fact_inv[(n - min(n, r)) as usize] % P
    }

    // p=N のとき u は根であり、p≠N のとき p は u の親である。
    // u と祖先は塗られていて、子孫は塗られていない。
    // u の部分木の残りの塗り方の総数を計算する。
    fn dfs(&self, u: usize, p: usize) -> i64 {
        // 注意: u がリーフノードなら何も塗らないの 1 通り
        let mut count = 1;

        // u の直接の子ノードの塗り方
        count *= {
            let K = self.K;

            // 子ノードの数
            let c = self.G[u].iter().filter(|&&v| v != p).count() as i64;
            // 使用できる色の数
            let d = if p == self.G.len() { K - 1 } else { K - 2 };

            self.perm(d, c)
        };
        count %= P;

        for i in 0..self.G[u].len() {
            let v = self.G[u][i];
            if v == p {
                continue;
            }

            // v の部分木を塗る。独立に塗れるので掛け算。
            count *= self.dfs(v, u);
            count %= P;
        }

        count
    }
}

fn main() {
    let (N, K) = read!(usize, i64);
    let mut G = vec![vec![]; N];

    for (u, v) in read![usize, usize; N - 1] {
        let (u, v) = (u - 1, v - 1);
        G[u].push(v);
        G[v].push(u);
    }

    let M = max(3, max(N, K as usize)) + 1;

    let mut fact = vec![0; M];
    fact[0] = 1;
    for i in 1..M {
        fact[i] = fact[i - 1] * (i as i64) % P;
    }

    let mut fact_inv = vec![0; M];
    for i in 0..M {
        fact_inv[i] = pow(fact[i], P - 2);
    }

    let mut count = Solver {
        K: K,
        G: G,
        fact: fact,
        fact_inv: fact_inv,
    }
    .dfs(0, N);

    // 根ノードを塗る。
    count *= K;
    count %= P;

    println!("{}", count)
}
