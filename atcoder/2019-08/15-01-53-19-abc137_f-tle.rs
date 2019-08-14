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

pub fn pow(x: i64, n: i64, P: i64) -> i64 {
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
    let P = read!(i64);
    let Pz = P as usize;
    let A = read![[i64]];

    let mut fact = vec![0; Pz];
    fact[0] = 1;
    fact[1] = 1;
    for n in 2..Pz {
        fact[n] = n as i64 * fact[n - 1] % P;
    }

    let mut fact_inv = vec![0; Pz];
    for n in 0..Pz {
        fact_inv[n] = pow(fact[n], P - 2, P);
    }

    let combo = |n: i64, k: i64| {
        let mut z = fact[n as usize];
        z *= fact_inv[k as usize];
        z %= P;
        z *= fact_inv[(n - k) as usize];
        z %= P;
        z
    };

    // f(x) = Σi. a(i)・(1 - (x-i)^(p-1))
    // (x-i)^(p-1) = Σk=0,p-1. C(p-1,k) x^k (-i)^(p-1-k)

    let mut B = vec![0; Pz];
    for i in 0..Pz {
        if A[i] == 0 {
            continue;
        }

        for k in 0..Pz {
            let mut b = combo(P - 1, k as i64);
            b *= pow(((Pz - i) % Pz) as i64, (Pz - 1 - k) as i64, P);
            b %= P;
            B[k] += P - b;
            B[k] %= P;
        }

        B[0] += 1;
    }

    println!(
        "{}",
        B.into_iter()
            .map(|b| (b % P).to_string())
            .collect::<Vec<_>>()
            .join(" ")
    )
}
