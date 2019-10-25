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

// Σ[i=0..n] Σ[j=i+1..n] Σ[k=0..m] Σ[l=k+1..m] (x(j) - x(i))(y(l) - y(k))
// =  (Σ[i=0..n] Σ[j=i+1..n] (x(j) - x(i)))
//  * (Σ[k=0..m] Σ[l=k+1..m] (y(l) - y(k)))

// Σ[i=0..n] Σ[j=i+1..n] (x(j) - x(i))
// = Σ[i=0..n] (i x(i) - (n - 1) x(i))
// = Σ[i=0..n] (2 i - N + 1) x(i)

const P: i64 = 1_000_000_007;

fn main() {
    let (N, M) = read!(usize, usize);
    let X = read![[i64]];
    let Y = read![[i64]];

    let mut s = 0;
    for i in 0..N {
        s += (2 * (i as i64) - (N as i64) + 1) * X[i] % P;
        s %= P;
    }

    let mut t = 0;
    for k in 0..M {
        t += (2 * (k as i64) - (M as i64) + 1) * Y[k] % P;
        t %= P;
    }

    println!("{}", (s * t) % P)
}
