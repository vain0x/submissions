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
    let S = rl().chars().collect::<Vec<_>>();
    let N = S.len();

    // dp[i][r] ≡ n (mod P):
    //   S[..i] の '?' に数字を埋めたときに得られる整数全体の集合を T としたとき、
    //   T の要素のうち 13 で割った余りが r に等しいものの集合の要素数が n に等しい
    let mut dp = vec![vec![0; 13]; N + 1];

    // 初期条件
    // 10進表記の "" が 0 を表すということにしておくと都合がいい。
    dp[0][0] = 1;

    for i in 0..N {
        // S[i] = d

        // 数字 d としてありえる値の最小値
        let u = if S[i] == '?' {
            0
        } else {
            (S[i] as u8 - b'0') as usize
        };

        // 数字 d の最大値
        let v = if S[i] == '?' {
            9
        } else {
            (S[i] as u8 - b'0') as usize
        };

        for r in 0..13 {
            for d in u..v + 1 {
                let s = (10 * r + d) % 13;
                dp[i + 1][s] += dp[i][r];
                dp[i + 1][s] %= P;
            }
        }
    }

    println!("{}", dp[N][5])
}
