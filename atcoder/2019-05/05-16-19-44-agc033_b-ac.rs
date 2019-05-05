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

/*
9 3 4
5 2
ULLR
RDDD
*/

fn main() {
    let (H, W, N) = read!(usize, usize, usize);
    let (Y, X) = read!(usize, usize);
    let S = rl().chars().collect::<Vec<_>>();
    let T = rl().chars().collect::<Vec<_>>();

    let mut safe = true;

    for &(X, W, R, L) in &[(X, W, 'R', 'L'), (Y, H, 'D', 'U')] {
        debug!((X, W, R, L));

        let mut l = 1;
        let mut r = W;

        for i in (0..N).rev() {
            // aoki
            if i != N - 1 {
                if T[i] == L && r + 1 <= W {
                    r += 1;
                }
                if T[i] == R && l - 1 >= 1 {
                    l -= 1;
                }
            }
            debug!((i, "aoki", l, r));

            // takahashi
            if S[i] == L {
                l += 1;
            }
            if S[i] == R {
                r -= 1;
            }
            safe = safe && 1 <= l && l <= r && r <= W;

            debug!((i, "taka", l, r));
        }

        safe = safe && l <= X && X <= r;
    }

    println!("{}", if safe { "YES" } else { "NO" })
}
