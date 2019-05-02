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
    let K = rl().chars().map(|c| c as u8 - b'0').collect::<Vec<u8>>();
    let D = read!(usize);

    let N = K.len();

    // dp[i][s][less] = x
    // 上位 i 桁が確定済み
    // 確定済みの桁の総和が s (mod D)
    // less = 1 <=> K 未満であることが確定済み
    // 数字の個数が x (mod P)
    let mut dp = vec![vec![vec![0, 0]; D]; N + 1];

    dp[0][0][0] = 1;

    for i in 0..N {
        for s in 0..D {
            for d in 0..10 {
                let less = K[i] > d as u8;
                let eq = K[i] == d as u8;

                dp[i + 1][(s + d) % D][1] += dp[i][s][1];
                dp[i + 1][(s + d) % D][1] %= P;

                if less {
                    dp[i + 1][(s + d) % D][1] += dp[i][s][0];
                    dp[i + 1][(s + d) % D][1] %= P;
                }
                if eq {
                    dp[i + 1][(s + d) % D][0] += dp[i][s][0];
                    dp[i + 1][(s + d) % D][0] %= P;
                }
            }
        }

        // debug!(dp[i + 1]);
    }

    let mut x = dp[N][0][0];
    x += dp[N][0][1];
    x %= P;

    // 0 を数えてしまっているので引く
    x += P - 1;
    x %= P;

    println!("{}", x)
}
