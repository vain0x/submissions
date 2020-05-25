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

fn f(s1: usize, s2: usize, s3: usize) -> usize {
    let s_max = max(s1, max(s2, s3));
    let s_min = min(s1, min(s2, s3));
    s_max - s_min
}

fn calc(H: usize, W: usize) -> usize {
    let mut min_d = std::usize::MAX;

    for i in 1..H {
        let s1 = i * W;

        // 平行に割る。
        for d in 0..5 {
            let j = (i + (H - i) / 2 + d).saturating_sub(2);
            if i < j && j < H {
                let s2 = (j - i) * W;
                let s3 = (H - j) * W;
                min_d = min(min_d, f(s1, s2, s3));
            }
        }

        // 垂直に割る。
        for d in 0..5 {
            let j = (W / 2 + d).saturating_sub(2);
            if 0 < j && j < W {
                let s2 = j * (H - i);
                let s3 = (W - j) * (H - i);
                min_d = min(min_d, f(s1, s2, s3));
            }
        }
    }

    min_d
}

fn main() {
    let (H, W) = read!(usize, usize);
    println!("{}", min(calc(H, W), calc(W, H)));
}
