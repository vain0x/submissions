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

pub fn factorize(mut x: i64) -> BTreeMap<i64, i64> {
    let mut ms = BTreeMap::new();
    let r = (x as f64).sqrt() as i64 + 1;

    for p in 2..r as i64 {
        let mut m = 0;

        while x >= p && x % p == 0 {
            x /= p;
            m += 1;
        }

        if m > 0 {
            ms.insert(p, m);
        }
    }

    // `x` can have a prime factor larger than √x at most one.
    if x > 1 {
        ms.insert(x, 1);
    }

    ms
}

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
    let N = read!(usize);
    let A = read![[i64]];
    let F = (0..N).map(|i| factorize(A[i])).collect::<Vec<_>>();

    let mut f = HashMap::new();
    for i in 0..N {
        for (&p, &m) in &F[i] {
            let r = f.entry(p).or_insert(0);
            let x = max(*r, m);
            *r = x;
        }
    }
    debug!(f.len());

    let mut s = 0_i64;
    for i in 0..N {
        let mut t = 1_i64;

        for (&p, &x) in &f {
            let m = x - F[i].get(&p).cloned().unwrap_or(0);
            t *= pow(p, m);
            t %= P;
        }

        s += t;
        s %= P;
    }

    println!("{}", s)
}
