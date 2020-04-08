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

/// 桁数
const W: usize = 19;

fn f(n: i64) -> i64 {
    let mut b = vec![0; W];
    let mut x = n;
    for i in 0..W {
        b[i] = x % 10;
        x /= 10;
    }
    b.reverse();

    // dp[i][eq] = combo
    // 上位 i 桁を確定した状態
    // 上位 i 桁が b[..i] に一致するとき eq=1
    // 残りの桁を埋めて作れる整数のうち 4/9 を *含まない* ものの個数が combo
    let mut dp = vec![vec![0; 2]; W + 1];
    dp[0][1] = 1_i64;

    for i in 0..W {
        for d in 0..10 {
            if d == 4 || d == 9 {
                continue;
            }

            dp[i + 1][0] += dp[i][0];

            if d < b[i] {
                dp[i + 1][0] += dp[i][1];
            }

            if d == b[i] {
                dp[i + 1][1] += dp[i][1];
            }
        }
    }

    n - dp[W][0]
}

fn main() {
    let (A, B) = read!(i64, i64);
    println!("{}", f(B + 1) - f(A));
}
