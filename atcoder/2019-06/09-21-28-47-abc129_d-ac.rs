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
    let (H, W) = read!(usize, usize);
    let S = read![String; H]
        .into_iter()
        .map(|S| S.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();

    // row[y][x] = yn : 位置 y, x に置くと同じ行の他の yn マスが照らされる
    let mut row = vec![vec![0; W]; H];

    for y in 0..H {
        let mut n = 0;
        let mut l = 0;
        for x in 0..W + 1 {
            if x == W || S[y][x] == '#' {
                for xx in l..x {
                    row[y][xx] = n;
                }
                n = 0;
                l = x + 1;
                continue;
            }

            n += 1;
        }
    }

    let mut col = vec![vec![0; W]; H];

    for x in 0..W {
        let mut n = 0;
        let mut l = 0;
        for y in 0..H + 1 {
            if y == H || S[y][x] == '#' {
                for yy in l..y {
                    col[yy][x] = n;
                }
                n = 0;
                l = y + 1;
                continue;
            }

            n += 1;
        }
    }

    let mut M = 0;
    for y in 0..H {
        for x in 0..W {
            M = max(M, row[y][x] + col[y][x] - 1);
        }
    }

    println!("{}", M)
}
