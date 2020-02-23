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

fn query(k: usize, d: &mut Vec<i64>, n: usize, mut x: i64, m: i64) -> usize {
    for i in 0..d.len() {
        d[i] %= m;
    }

    x %= m;

    let a_last = {
        let mut a = 0;

        for i in 0..k {
            let mut t = n / k;
            t += if i < n % k { 1 } else { 0 };

            a += (t as i64) * d[i];
        }
        a
    };

    // a[j] mod m == a[j+1] mod m
    // <=> d[j] == 0
    let u = (0..k)
        .map(|i| {
            if d[i] != 0 {
                return 0;
            }

            let mut t = n / k;
            t += if i < n % k { 1 } else { 0 };
            t
        })
        .sum::<usize>();

    // a[j] mod m > a[j+1] mod m
    // となるのは a[j] を m で割った商が 1 増えるとき
    let v = (a_last / m - x / m) as usize;

    (n - 1) - (u + v)
}

fn main() {
    let (k, q) = read!(usize, usize);
    let d = read![[i64]];
    let T = read![usize, i64, i64; q];

    let mut d_buf = d.clone();

    for (n, x, m) in T {
        d_buf.clear();
        d_buf.extend(d.iter().cloned());

        println!("{}", query(k, &mut d_buf, n, x, m));
    }
}
