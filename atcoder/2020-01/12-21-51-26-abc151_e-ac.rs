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
    let (N, K) = read!(usize, usize);
    let mut A = read![[i64]];
    A.sort();

    let M = N + K;
    let inv = inv_dp(M);

    let mut fact = vec![0; M];
    fact[0] = 1;
    fact[1] = 1;
    for i in 2..fact.len() {
        fact[i] = i as i64 * fact[i - 1] % P;
    }

    let mut fact_inv = vec![0; M];
    fact_inv[0] = 1;
    fact_inv[1] = 1;
    for i in 2..fact_inv.len() {
        fact_inv[i] = inv[i] * fact_inv[i - 1] % P;
    }

    let C = |n: usize, r: usize| {
        if n < r {
            return 0;
        }

        let mut c = fact[n];
        c *= fact_inv[n - r];
        c %= P;
        c *= fact_inv[r];
        c % P
    };

    let mut sum = 0_i64;
    for i in 0..N {
        // max 側
        sum += C(i, K - 1) * A[i];

        debug!(i, C(i, K - 1), A[i], C(i, K - 1) * A[i]);

        // min 側
        sum -= C(N - i - 1, K - 1) * A[i];
        debug!((C(N - i - 1, K - 1) * A[i]));

        sum %= P;
    }

    sum += P;
    sum %= P;

    println!("{}", sum)
}

/*

5 3
1 1 2 3 5
==> 30

*/
