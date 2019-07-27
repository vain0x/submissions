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

fn main() {
    let S = rl();
    let N = S.len();

    let S = std::iter::repeat('0')
        .take((3 - N % 3) % 3)
        .chain(S.chars())
        .collect::<Vec<_>>();
    let N = S.len();
    debug_assert_eq!(N % 3, 0);

    debug!(S, N);

    // 数字を下から3桁ごとに分けたとき各部分をブロックと呼ぶ
    // 前方のブロックの i (≥ 0) 番目の値を x とするとき t = Σ(-1)^i x とおく
    // dp[i][t] = n
    let mut dp = vec![vec![0; 13]; N / 3 + 1];

    dp[0][0] = 1;

    for bi in 0..N / 3 {
        for x in 0..13 {
            fn lb(c: char) -> usize {
                if c == '?' {
                    0
                } else {
                    (c as u8 - b'0') as usize
                }
            }
            fn ub(c: char) -> usize {
                if c == '?' {
                    10
                } else {
                    (c as u8 - b'0' + 1) as usize
                }
            }

            for d1 in lb(S[bi * 3])..ub(S[bi * 3]) {
                for d2 in lb(S[bi * 3 + 1])..ub(S[bi * 3 + 1]) {
                    for d3 in lb(S[bi * 3 + 2])..ub(S[bi * 3 + 2]) {
                        let y = d1 * 100 + d2 * 10 + d3;
                        let z = (13 + y - x) % 13;
                        dp[bi + 1][z] += dp[bi][x];
                        dp[bi + 1][z] %= P;
                    }
                }
            }
        }
    }

    println!("{}", dp[N / 3][5])
}
