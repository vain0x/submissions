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

const P: i64 = 998244353;

fn main() {
    let N = read!(usize);
    let A = read![usize; N];
    let S = A.iter().sum::<usize>();
    let M = A.iter().cloned().max().unwrap();

    // dp1[k][r] = x : 前から k 個の整数を塗り分けて、赤の総和が r になる塗りかたの個数が x
    let mut dp1 = vec![vec![0; M * (N + 1) + 1]; N + 1];
    // dp2[k][r] = x : ある1色を塗らない場合
    let mut dp2 = vec![vec![0; M * (N + 1) + 1]; N + 1];
    dp1[0][0] = 1;
    dp2[0][0] = 1;

    for k in 0..N {
        let a = A[k];
        for r in 0..M * N + 1 {
            // この整数を赤に塗るケース
            dp1[k + 1][r + a] += dp1[k][r];
            dp1[k + 1][r] %= P;
            dp2[k + 1][r + a] += dp2[k][r];
            dp2[k + 1][r] %= P;

            // 緑か青に塗るケース
            dp1[k + 1][r] += 2 * dp1[k][r];
            dp1[k + 1][r] %= P;
            dp2[k + 1][r] += 1 * dp2[k][r];
            dp2[k + 1][r] %= P;
        }
    }

    let mut u = 0;
    for r in (S + 1) / 2..M * N + 1 {
        u += dp1[N][r];
        u %= P;
    }
    u *= 3;
    u %= P;

    let mut v = if S % 2 == 0 { dp2[N][S / 2] } else { 0 };
    v *= 3;
    v %= P;

    let mut p = 1;
    for _ in 0..N {
        p *= 3;
        p %= P;
    }

    let n = ((p + (P - u)) % P + v) % P;
    debug!(p, u, v);

    println!("{}", n)
}
