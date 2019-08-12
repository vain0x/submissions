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

fn divisors(x: i64) -> Vec<i64> {
    let mut d = 1;
    let mut ds = vec![];
    while d * d <= x {
        if x % d == 0 {
            ds.push(d);
            ds.push(x / d);
        }
        d += 1
    }
    ds.sort();
    ds.dedup();
    ds
}

// x を d で割った余り
fn rep(x: i64, d: i64) -> i64 {
    x % d
}

// x に足すと d の倍数になる最小値
fn complement(x: i64, d: i64) -> i64 {
    (x - x % d) % d
}

fn solve(A: &mut [i64], K: i64) -> i64 {
    let N = A.len();
    let sum = A.iter().sum::<i64>();

    for d in divisors(sum).into_iter().rev() {
        // d で割った余りだけ考える。
        // 余りの小さい値を -1 して、大きい値を +1 するため、余りに関して昇順にする。
        A.sort_by(|&l, &r| rep(l, d).cmp(&(rep(r, d))));

        // A[..i] を減少させる量
        let mut neg = 0;
        // A[i..] を増加させる量
        let mut pos = A.iter().map(|&a| complement(a, d)).sum::<i64>();

        for i in 0..N + 1 {
            // 減少量と増加量が一致して操作回数がどちらも K 以下なら操作が存在する
            if neg <= K && pos <= K && neg == pos {
                return d;
            }

            if i < N {
                neg += A[i] % d;
                pos -= complement(A[i], d);
            }
        }
    }

    1
}

fn main() {
    let (_, K) = read!(usize, i64);
    let mut A = read![[i64]];
    println!("{}", solve(&mut A, K))
}
