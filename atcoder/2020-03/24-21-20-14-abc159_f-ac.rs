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

const P: usize = 998244353;

fn main() {
    let (N, S) = read!(usize, usize);
    let A = read![[usize]];

    // (L, x, R) (ただし L≤x(0) < ... < x(k) < R) の個数。
    let mut total = 0_usize;

    // dp[i][s] = (状態の数)
    // 状態:
    //     L と x(i) (L 以上 i 未満) の一部を決定済み
    //     A(x(i)) の総和が s に等しい
    let mut dp = vec![vec![0; S + 1]; N + 1];

    for i in 0..N {
        // (L=i, x=(), R=?) がある。
        dp[i][0] = 1;
    }

    for i in 0..N {
        let a = A[i];

        for s in 0..S + 1 {
            // 列 x に i を加えない遷移
            dp[i + 1][s] += dp[i][s];

            // 列 x に i を加える遷移。総和が S を超えるケースは数える意味がないので無視。
            if s + a <= S {
                dp[i + 1][s + a] += dp[i][s];
            }
        }

        for s in 0..S + 1 {
            dp[i + 1][s] %= P;
        }

        // R=i+1 に決定する遷移。総和が S でないケースは数える意味がないので無視。
        total += dp[i + 1][S];
        total %= P;
    }

    println!("{}", total);
}
