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

fn main() {
    let (n, k) = read!(usize, usize);
    let M = n * 2;

    let mut fact = vec![0; M];
    fact[0] = 1_i64;
    fact[1] = 1;
    for i in 2..M {
        fact[i] = (i as i64) * fact[i - 1] % P;
    }

    let mut fact_inv = vec![0; M];
    fact_inv[M - 1] = pow(fact[M - 1], P - 2);

    for i in (1..M).rev() {
        fact_inv[i - 1] = (i as i64) * fact_inv[i] % P;
    }

    let mut s = 0;

    for z in 0..min(n, k + 1) {
        // C(n, z) * (z + (n - z - 1))! / z! * (n - z - 1)!)
        let mut c = fact[n];
        c *= fact_inv[z];
        c %= P;
        c *= fact_inv[n - z];
        c %= P;

        let mut t = c;
        t *= fact[z + (n - z - 1)];
        t %= P;
        t *= fact_inv[z];
        t %= P;
        t *= fact_inv[n - z - 1];
        t %= P;

        s += t;
        s %= P;
    }

    println!("{}", s)
}
