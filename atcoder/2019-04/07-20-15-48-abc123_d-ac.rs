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
    let (_, _, _, K) = read!(usize, usize, usize, usize);
    let mut A = read![[i64]];
    let mut B = read![[i64]];
    let mut C = read![[i64]];

    A.sort_by(|l, r| r.cmp(l));
    B.sort_by(|l, r| r.cmp(l));
    C.sort_by(|l, r| r.cmp(l));

    let mut S = vec![];

    for a in 0..A.len() {
        for b in 0..B.len() {
            if (a + 1) * (b + 1) - 1 >= K {
                // このとき (a, b, c) の組み合わせのうち a, b の値がいまの a, b より小さいようなものが K 個以上ある。
                // そのような組み合わせはいずれもいまの (a, b, _) 以上の和になる。 (c として最大値を選べばいい)
                // だからいまの a, b を使った組み合わせ (a, b, _) は上位 K 個に入らないと考えていい。
                break;
            }

            for c in 0..C.len() {
                if (a + 1) * (b + 1) * (c + 1) - 1 >= K {
                    break;
                }

                let sum = A[a] + B[b] + C[c];
                S.push(sum);
            }
        }
    }

    S.sort_by(|l, r| r.cmp(l));
    debug!(S);

    for s in S.into_iter().take(K) {
        println!("{}", s);
    }
}
