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
    let (N, M, K) = read!(usize, usize, usize);
    let NM = N * M;

    // fact[n] = n! (mod P)
    let mut fact = vec![0; NM + 1];
    fact[0] = 1;
    fact[1] = 1;
    for n in 2..NM + 1 {
        // n! = (n - 1!) * n
        fact[n] = fact[n - 1] * n as i64 % P;
    }

    // inv[n] = 1/n (mod P)
    let mut inv = vec![0; NM + 1];
    inv[1] = 1;
    for n in 2..NM + 1 {
        // 1/n = -(P / n) / (P % n)
        // (P = floor(P / n) n + P % n から導出)
        let mut z = P - inv[(P % n as i64) as usize];
        z %= P;
        z *= P / n as i64;
        z %= P;
        inv[n] = z;
    }

    // fact_inv[n] = 1/(n!) (mod P)
    let mut fact_inv = vec![0; NM + 1];
    fact_inv[0] = 1;
    fact_inv[1] = 1;
    for n in 2..NM + 1 {
        // 1/(n!) = (1 / (n - 1)!) * 1/n
        fact_inv[n] = fact_inv[n - 1] * inv[n] % P;
    }

    // C(n, r) = n! / ((n - r)! r!)
    let combo = |n, r| {
        let mut z = fact_inv[n - r];
        z *= fact_inv[r];
        z %= P;
        z *= fact[n];
        z %= P;
        z
    };

    // M^2 * Σd. d (N - d)
    let mut y = 0;
    for d in 1..N {
        y += (d * (N - d)) as i64 % P;
        y %= P;
    }
    y *= (M * M) as i64;
    y %= P;

    // N^2 * Σd. d (M - d)
    let mut x = 0;
    for d in 1..M {
        x += (d * (M - d)) as i64 % P;
        x %= P;
    }
    x *= (N * N) as i64 % P;
    x %= P;

    let mut z = (y + x) % P;
    z *= combo(NM - 2, K - 2);
    z %= P;

    println!("{}", z)
}
