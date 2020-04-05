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

pub fn next_perm<T: Ord>(xs: &mut [T]) -> bool {
    // `xs[i + 1..]` : desc but
    // `xs[i..]` : not desc.
    let i = match (0..xs.len())
        .rev()
        .filter(|&i| i + 1 < xs.len() && xs[i] < xs[i + 1])
        .next()
    {
        None => return false,
        Some(i) => i,
    };

    // `xs[k]` : The next greater elem in `xs[i..]`.
    let k = (i + 1..xs.len())
        .rev()
        .filter(|&k| xs[i] < xs[k])
        .next()
        .unwrap();

    // E.g. 2431 -> 3421 -> 3124 (where i = 0, k = 2).
    xs.swap(i, k);
    xs[i + 1..].reverse();

    true
}

fn main() {
    let A = read![[usize]];

    let N = A.iter().sum::<usize>();

    let mut rev = HashMap::new();
    let mut i = 0;
    for x in 0..A.len() {
        for y in 0..A[x] {
            rev.insert((x, y), i);
            i += 1;
        }
    }

    let mut p = (0..N).collect::<Vec<_>>();
    let mut n = 0_usize;
    loop {
        let mut ok = true;
        for x in 0..A.len() {
            for y in 0..A[x] {
                if y >= 1 && p[rev[&(x, y)]] <= p[rev[&(x, y - 1)]] {
                    ok = false;
                    break;
                }

                if x >= 1 && p[rev[&(x, y)]] <= p[rev[&(x - 1, y)]] {
                    ok = false;
                    break;
                }
            }
        }

        if ok {
            n += 1;
        }

        if !next_perm(&mut p) {
            break;
        }
    }

    println!("{}", n)
}
