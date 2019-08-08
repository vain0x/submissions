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

fn main() {
    // 考察:
    // RL の部分は交互に入れ替わり続けて、
    // RL 以外の部分は最終的に RL の左右どちらかに合流する
    // どっちに合流するか RL との距離の偶奇と方向で決まる?
    // RL の左側にある R は近い方から B, A, B, A.. と塗る
    // RL の右側にある L は近い方から A, B, A, B.. と塗る
    // A なら RL の左側 +1
    // B なら RL の右側 +1

    let S = rl().chars().collect::<Vec<_>>();
    let N = S.len();

    let mut i = 0;
    let mut X = vec![0; N];
    while i + 1 < N {
        if S[i] == 'R' && S[i + 1] == 'L' {
            X[i] += 1;
            X[i + 1] += 1;

            let mut k = 1;
            for j in (0..i).rev() {
                if S[j] != 'R' {
                    break;
                }
                X[i + k] += 1;
                k ^= 1;
            }

            let mut k = 0;
            for j in (i + 2)..N {
                if S[j] != 'L' {
                    break;
                }
                X[i + k] += 1;
                k ^= 1;
            }

            i += 2;
            continue;
        }

        i += 1;
    }

    println!(
        "{}",
        X.into_iter()
            .map(|x| x.to_string())
            .collect::<Vec<_>>()
            .join(" ")
    )
}
