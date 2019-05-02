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

struct Solver {
    N: usize,
    S: Vec<char>,
    combo: Vec<Vec<i64>>,
    dp: Vec<Vec<Option<i64>>>,
}

impl Solver {
    fn dfs(&mut self, l: usize, r: usize) -> i64 {
        if l == r {
            return 1;
        }
        if let Some(x) = self.dp[l][r] {
            return x;
        }

        let x = self.do_dfs(l, r);
        self.dp[l][r] = Some(x);
        x
    }

    // 区間 l..r に (r - l) 種類の数値を配置するときの場合の数を計算する
    fn do_dfs(&mut self, l: usize, r: usize) -> i64 {
        let mut x = 0;

        // 最小の数値をどこに配置するかを全通り試す
        for m in l..r {
            // 残りの数値のうち (m - l) 種類を左側に配置する
            // 具体的にどの数値を左側に置くか、の場合の数
            let c = self.combo[r - l - 1][m - l];

            // 残りの (m - l) 種類を左側に配置する
            // 左側に配置される数字はいま位置 m に配置した数字より大きいので、条件は満たされていなければいけない
            let lx = if m > l && m >= 1 && self.S[m - 1] == '<' {
                0
            } else {
                self.dfs(l, m)
            };

            // 残りの (r - (m + 1)) 種類を右側に配置する
            let rx = if m + 1 < r && m < self.S.len() && self.S[m] == '>' {
                0
            } else {
                self.dfs(m + 1, r)
            };

            let mut y = c;
            y *= lx;
            y %= P;
            y *= rx;
            y %= P;

            debug!((l, r, m, c, lx, rx, y));

            x += y;
            x %= P;
        }
        x
    }

    fn solve(mut self) {
        let N = self.N;

        for n in 0..N + 1 {
            for r in 0..n + 1 {
                if r == 0 || r >= n {
                    self.combo[n][r] = 1;
                    continue;
                }
                self.combo[n][r] += self.combo[n - 1][r - 1];
                self.combo[n][r] += self.combo[n - 1][r];
                self.combo[n][r] %= P;
            }
        }
        debug!(self.combo);

        println!("{}", self.dfs(0, N));
    }
}

fn main() {
    let N = read!(usize);
    let S = rl().chars().collect::<Vec<_>>();

    Solver {
        N: N,
        S: S,
        combo: vec![vec![0; N + 1]; N + 1],
        dp: vec![vec![None; N + 2]; N + 2],
    }
    .solve()
}
