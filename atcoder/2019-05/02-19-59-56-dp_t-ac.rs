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

#[derive(Clone, Copy)]
enum Cmp {
    Lt,
    Gt,
}

fn cmp_to_index(c: Cmp) -> usize {
    match c {
        Cmp::Lt => 0,
        Cmp::Gt => 1,
    }
}

struct Solver {
    N: usize,
    S: Vec<char>,
    dp: Vec<Vec<Vec<Option<i64>>>>,
}

impl Solver {
    fn dfs(&mut self, i: usize, k: usize, c: Cmp) -> i64 {
        if let Some(x) = self.dp[i][k][cmp_to_index(c)] {
            return x;
        }

        let x = self.do_dfs(i, k, c);
        self.dp[i][k][cmp_to_index(c)] = Some(x);
        x
    }

    // 0..i に配置する数字が決定していて、
    // i 番目に配置できる数字が k 通りあり、
    // 直前の数字と i 番目の数字の大小関係が c であるとき、
    // 残りの順列を埋める場合の数を求める
    fn do_dfs(&mut self, i: usize, k: usize, c: Cmp) -> i64 {
        if i >= self.N {
            return 1;
        }
        if i == self.N - 1 {
            return k as i64;
        }
        if k == 0 {
            return 0;
        }

        let m = self.N - i;
        let mut x = 0;
        let d = if self.S[i] == '<' { Cmp::Lt } else { Cmp::Gt };
        match (c, d) {
            (Cmp::Lt, Cmp::Lt) => {
                x += self.dfs(i + 1, k - 1, Cmp::Lt);
                x += self.dfs(i, k - 1, Cmp::Lt);
                x %= P;
            }
            (Cmp::Lt, Cmp::Gt) => {
                x += self.dfs(i + 1, m - k, Cmp::Gt);
                x += self.dfs(i, k - 1, Cmp::Lt);
                x %= P;
            }
            (Cmp::Gt, Cmp::Lt) => {
                x += self.dfs(i + 1, m - k, Cmp::Lt);
                x += self.dfs(i, k - 1, Cmp::Gt);
                x %= P;
            }
            (Cmp::Gt, Cmp::Gt) => {
                x += self.dfs(i + 1, k - 1, Cmp::Gt);
                x += self.dfs(i, k - 1, Cmp::Gt);
                x %= P;
            }
        }
        x
    }

    fn solve(mut self) {
        let N = self.N;

        println!("{}", self.dfs(0, N, Cmp::Lt));
    }
}

fn main() {
    let N = read!(usize);
    let S = rl().chars().collect::<Vec<_>>();

    Solver {
        N: N,
        S: S,
        dp: vec![vec![vec![None; 2]; N + 2]; N + 2],
    }
    .solve()
}
