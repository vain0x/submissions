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

fn decode(i: usize) -> char {
    match i {
        0 => ' ',
        1 => 'A',
        2 => 'C',
        3 => 'G',
        4 => 'T',
        _ => unreachable!(),
    }
}

fn is_banned(x: usize, y: usize, z: usize, w: usize) -> bool {
    match (decode(x), decode(y), decode(z), decode(w)) {
        ('A', 'G', _, 'C')
        | ('A', _, 'G', 'C')
        | (_, 'A', 'G', 'C')
        | (_, 'A', 'C', 'G')
        | (_, 'G', 'A', 'C') => true,
        _ => false,
    }
}

fn main() {
    let N = read!(usize);

    // dp[i][x][y][z] = 場合の数
    // 前から i 文字が確定済み
    // i-3, i-2, i-1 文字目がそれぞれ x, y, z

    let mut dp = vec![vec![vec![vec![0; 5]; 5]; 5]; N + 1];
    let mut ok = vec![vec![vec![vec![false; 5]; 5]; 5]; N + 1];

    dp[0][0][0][0] = 1_i64;
    ok[0][0][0][0] = true;

    for i in 0..N {
        for x in 0..5 {
            for y in 0..5 {
                for z in 0..5 {
                    if !ok[i][x][y][z] {
                        continue;
                    }

                    for w in 1..5 {
                        if is_banned(x, y, z, w) {
                            continue;
                        }

                        dp[i + 1][y][z][w] += dp[i][x][y][z];
                        ok[i + 1][y][z][w] = true;
                    }
                }
            }
        }

        for y in 0..5 {
            for z in 0..5 {
                for w in 0..5 {
                    dp[i + 1][y][z][w] %= P;
                }
            }
        }
    }

    debug!(dp, ok);

    let mut total = 0_i64;

    for x in 1..5 {
        for y in 1..5 {
            for z in 1..5 {
                if ok[N][x][y][z] {
                    total += dp[N][x][y][z];
                }
            }
        }
    }

    println!("{}", total % P)
}
