// -----------------------------------------------
// Framework <https://github.com/vain0x/procon>
//
// See the bottom of file for solution.
// -----------------------------------------------

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
    buf.trim_right().to_owned()
}

// -----------------------------------------------
// Solution
// -----------------------------------------------
fn popcount(mut x: i64) -> i64 {
    let mut n = 0_i64;
    while x > 0 {
        n += x & 1;
        x >>= 1;
    }
    n
}

fn main() {
    let (N, A, B, C) = read!(usize, i64, i64, i64);
    let T = read![i64; N];

    let mut min_cost = 1_i64 << 60;

    let calc = |s: i64, L: i64| {
        let mut cost = 0;
        let mut len = 0;

        cost += (popcount(s) - 1) * 10;
        for i in 0..N {
            if s & (1 << i) != 0 {
                len += T[i];
            }
        }
        cost += (L - len).abs();
        cost
    };

    for c in 1..(1 << N) {
        let c_cost = calc(c, C);

        for b in 1..(1 << N) {
            if b & c != 0 {
                continue;
            }

            let b_cost = calc(b, B);

            for a in 1..(1 << N) {
                if a & b != 0 || a & c != 0 {
                    continue;
                }
                let a_cost = calc(a, A);

                min_cost = min(min_cost, a_cost + b_cost + c_cost);
            }
        }
    }

    println!("{}", min_cost)
}
