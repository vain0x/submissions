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

/// Calculates `x^n`. O(log n) time.
/// By Fermat's little theorem, `x^(-1) = pow(x, P - 2)`.
pub fn pow(x: i64, n: i64) -> i64 {
    let (mut x, mut y, mut n) = (x % P, 1_i64, n);
    while n > 0 {
        if n % 2 != 0 {
            y = (y * x) % P;
            n -= 1;
        }

        x = (x * x) % P;
        n /= 2;
    }
    y
}

/// Calculates `1/a` for each `a` in `1..n`.
/// Use `P = floor(P / k) * k + P % k` for proof.
pub fn inv_dp(n: usize) -> Vec<i64> {
    let mut dp = vec![0; n];
    if n >= 2 {
        dp[1] = 1;
        for i in 2..n {
            let mut z = P - dp[(P % i as i64) as usize];
            z %= P;
            z *= P / i as i64;
            z %= P;
            dp[i] = z;
        }
    }
    dp
}

fn main() {
    let (r1, c1, r2, c2) = read!(usize, usize, usize, usize);

    let M = (r2 + c2 + 1) * 2 as usize;

    let mut fact = vec![0; M];
    fact[0] = 1;
    fact[1] = 1;

    for i in 2..M {
        fact[i] = i as i64 * fact[i - 1] % P;
    }

    let inv = inv_dp(M);

    let mut fact_inv = vec![0; M];
    fact_inv[0] = 1;
    fact_inv[1] = 1;

    for i in 2..M {
        fact_inv[i] = inv[i] * fact_inv[i - 1] % P;
    }

    let mut s = 0_i64;

    for i in r1..r2 + 1 {
        let c = c1 as i64;
        let d = c2 as i64;

        let u = (d + 1) * fact[(d + i as i64 + 1) as usize] % P * fact_inv[(d + 1) as usize] % P;
        let v = c * fact[(c + i as i64) as usize] % P * fact_inv[c as usize] % P;

        let mut t = (u + P - v) * inv[i + 1] % P;
        t *= fact_inv[i];
        t %= P;

        s += t;
    }

    println!("{}", s % P)
}
