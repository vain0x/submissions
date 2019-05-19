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

#[allow(unused)]
fn check(M: usize, K: i64, v: &[i64]) -> bool {
    assert_eq!(v.len(), 1 << (M + 1));

    for i in 0..v.len() {
        for j in i + 2..v.len() {
            if v[i] != v[j - 1] {
                continue;
            }
            let mut x = 0;
            for k in i..j {
                x ^= v[k];
            }
            if x != K {
                debug!((i, j), x);
                return false;
            }
        }
    }
    true
}

fn main() {
    // assert!(check(2, 3, &vec![0, 1, 2, 0, 3, 2, 1, 3]));
    // assert!(check(2, 3, &vec![0, 3, 0, 1, 2, 3, 2, 1]));
    // assert!(check(
    //     3,
    //     1,
    //     &vec![0, 1, 0, 7, 6, 5, 4, 3, 2, 1, 2, 3, 4, 5, 6, 7]
    // ));

    let (M, K) = read!(usize, i64);
    // debug!((M, K), 1 << M, 1 << (M + 1));
    if K >= (1 << M) {
        println!("-1");
        return;
    }

    let mut v = Vec::with_capacity(1 << (M + 1));
    if K == 0 {
        for i in 0..(1 << M) {
            v.push(i);
            v.push(i);
        }
    } else {
        v.extend(&[0, K, 0]);
        for i in 1..(1 << M) {
            if i == K {
                continue;
            }
            v.push(i);
        }
        v.push(K);
        for i in (1..(1 << M)).rev() {
            if i == K {
                continue;
            }
            v.push(i);
        }
    }

    // debug_assert!(check(M, K, &v));

    println!(
        "{}",
        v.into_iter()
            .map(|a| a.to_string())
            .collect::<Vec<_>>()
            .join(" ")
    )
}
