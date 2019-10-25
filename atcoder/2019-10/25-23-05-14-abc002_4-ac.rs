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

fn pop_count(mut s: usize) -> usize {
    let mut count = 0;
    while s > 0 {
        if (s & 1) != 0 {
            count += 1;
        }
        s >>= 1;
    }
    count
}

fn main() {
    let (N, M) = read!(usize, usize);

    // G[u]: u および u と直接の知り合いである議員の集合 (ビットセット)
    let mut G = vec![0; N];
    for u in 0..N {
        G[u] |= 1 << u;
    }

    for (u, v) in read![usize, usize; M] {
        let (u, v) = (u - 1, v - 1);
        G[u] |= 1 << v;
        G[v] |= 1 << u;
    }

    // 派閥の最大サイズ
    let mut m = 1;

    for s in 1..(1 << N) {
        if pop_count(s) <= m {
            continue;
        }

        // 派閥に属する議員の集合
        let mut t = s;

        // 派閥に属する各議員 u について、
        // u と知り合いでない議員を集合から除去する。
        for u in 0..N {
            if s & (1 << u) != 0 {
                t &= G[u];
            }
        }

        // 誰も除去されなければ、すべての議員が互いに知り合いといえる。
        if t == s {
            m = pop_count(s);
        }
    }

    println!("{}", m)
}
