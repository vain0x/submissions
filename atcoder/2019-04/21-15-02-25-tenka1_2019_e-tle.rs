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

pub fn gcd(x: i64, y: i64) -> i64 {
    if y == 0 {
        x.abs()
    } else {
        gcd(y, x % y)
    }
}

/// Performs the sieve of Eratosthenes.
/// O(n log (log n)) time.
pub fn sieve(n: usize) -> Vec<bool> {
    let mut sieve = vec![true; max(2, n)];
    sieve[0] = false;
    sieve[1] = false;

    for p in 2..n {
        if !sieve[p] {
            continue;
        }

        let mut k = 2 * p;
        while k < n {
            sieve[k] = false;
            k += p;
        }
    }

    sieve
}

/// Performs prime factorization in O(√x) time.
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

// mod p において多項式 A が x^p - x で割り切れるならOK
fn ok(p: usize, A: &[i64]) -> bool {
    let mut B = A.to_owned();
    let N = A.len() - 1;

    for k in (0..N + 1).rev() {
        debug!((k, p), B);

        // -c * x^(k - p) * (x^p - x) を足して x^k の項を消す。
        if k >= p {
            let c = B[k];
            B[k] = 0;
            B[k - p + 1] += c;
        }

        if B[k] % (p as i64) != 0 {
            return false;
        }
    }
    true
}

fn main() {
    let N = read!(usize);
    let mut A = read![i64; N + 1];
    A.reverse();

    // 係数の最大公約数 g
    // f(x) は明らかに g の倍数になるので g の素因数はすべて解に含まれる。
    let mut g = A[0];
    for &a in &A {
        g = gcd(g, a);
    }
    let g_factors = factorize(g);

    // 係数列が互いに素になるようにしておく
    for a in &mut A {
        *a /= g;
    }

    debug!(g_factors);

    // 2..=N の素数については全通り試す
    let sieve = sieve(N + 1);

    let mut ps = (2..N + 1)
        .filter(|&p| sieve[p] && ok(p, &A))
        .chain(g_factors.keys().map(|&p| p as usize))
        .collect::<Vec<_>>();
    ps.sort();
    ps.dedup();

    for p in ps {
        println!("{}", p);
    }
}
