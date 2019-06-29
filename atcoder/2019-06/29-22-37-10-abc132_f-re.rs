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

const P: usize = 1_000_000_007;

fn find_r(x: usize, N: usize) -> usize {
    // N/y < N/x となる最小の y を探す

    let mut l = x; // ng
    let mut r = N + 1; // ok
    while r - l > 1 {
        let m = (l + r) / 2;
        // debug!((x, N, l, r, m, N / l, N / m));

        if N / m < N / l {
            r = m;
        } else {
            l = m;
        }
    }

    r
}

fn main() {
    let (N, K) = read!(usize, usize);

    let mut mul = vec![];
    let mut val = vec![];

    let mut l = 1;
    while l < N {
        let r = find_r(l, N);
        mul.push(r - l);
        val.push(N / l);
        l = r;
    }
    // while r <= N + 1 {
    //     if r == N + 1 || N / r != N / l {
    //         mul.push(r - l);
    //         val.push(N / l);
    //         l = r;
    //         r = l + 1;
    //         continue;
    //     }

    //     r += 1;
    // }
    // debug!(mul);

    let R = mul.len();

    let k = 2;
    let mut inc = vec![0; R];
    let mut acc = vec![0; R];

    for r in 0..R {
        inc[r] = val[r] % P;
    }

    acc[0] = inc[0];
    for r in 1..R {
        acc[r] = (acc[r - 1] + (mul[r] % P * inc[r] % P)) % P;
    }

    // debug!(k, inc, acc);

    for k in 3..K + 1 {
        for r in 0..R {
            inc[r] = acc[R - 1 - r];
        }

        acc[0] = inc[0];
        for r in 1..R {
            acc[r] = (acc[r - 1] + (mul[r] % P * inc[r] % P)) % P;
        }

        // debug!(k, inc, acc);
    }

    println!("{}", acc[R - 1])
}
