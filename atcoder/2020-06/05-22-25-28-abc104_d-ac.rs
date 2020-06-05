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

    let mut k = 1;
    let mut A = 0;
    let mut AB = 0;
    let mut ABC = 0;

    for c in S.chars() {
        match c {
            'A' => {
                // 左にある ? の数 Q に対して 3^Q 個の重複がある。
                A += k;
                A %= P;
            }
            'B' => {
                AB += A;
                AB %= P;
            }
            'C' => {
                ABC += AB;
                ABC %= P;
            }
            '?' => {
                // ? を A, B, C のどれに選ぶかによって、左にある ABC の選び方は3倍に増える。
                // さらに C を選んだときのみ、左にある AB の選び方だけ ABC の選び方が増える。
                ABC *= 3;
                ABC %= P;
                ABC += AB;
                ABC %= P;

                AB *= 3;
                AB %= P;
                AB += A;
                AB %= P;

                A *= 3;
                A %= P;
                A += k;
                A %= P;

                k *= 3;
                k %= P;
            }
            _ => unreachable!(),
        }
    }

    println!("{}", ABC)
}
