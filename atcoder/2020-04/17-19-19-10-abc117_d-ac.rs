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

/// ビット数
const H: usize = 50;

fn main() {
    let (_, K) = read!(usize, usize);
    let A = read![[i64]];

    // sum[h][b] = s
    // X の上から h ビット目を b にしたとき、(X ^ A[i]) のそのビットの総和が s に等しい
    let mut sum = vec![vec![0_i64; 2]; H];

    for &a in &A {
        for h in 0..H {
            sum[h][0] += a & (1 << (H - h - 1));
            sum[h][1] += (!a) & (1 << (H - h - 1));
        }
    }

    // dp[h][eq] = m
    //
    // 状態:
    // - X の上位 h ビットが確定済み。
    // - eq = 1 <=> X と K の上位 h ビットが一致
    //
    // 計算値:
    // - m はこの状態における (X ^ A[i]) の上位 h ビットの総和の最大値
    let mut dp = vec![vec![0_i64; 2]; H + 1];

    // ok[h][eq] = (状態 (h, eq) に遷移可能？)
    let mut ok = vec![vec![false; 2]; H + 1];
    ok[0][1] = true;

    for h in 0..H {
        if ok[h][0] {
            for b in 0..2 {
                dp[h + 1][0] = max(dp[h + 1][0], dp[h][0] + sum[h][b]);
                ok[h + 1][0] = true;
            }
        }

        if ok[h][1] {
            let kb = (K >> (H - h - 1)) & 1;
            if kb == 1 {
                // K 未満のビット (0) を立てるケース
                dp[h + 1][0] = max(dp[h + 1][0], dp[h][1] + sum[h][0]);
                ok[h + 1][0] = true;
            }

            // K と同じビットを立てるケース
            dp[h + 1][1] = max(dp[h + 1][1], dp[h][1] + sum[h][kb]);
            ok[h + 1][1] = true;
        }
    }

    println!("{}", max(dp[H][0], dp[H][1]));
}
