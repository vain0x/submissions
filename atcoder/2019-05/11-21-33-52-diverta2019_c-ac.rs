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

fn main() {
    let N = read!(usize);
    let S = (0..N).map(|_| rl()).collect::<Vec<_>>();

    let mut a = 0;
    let mut b = 0;
    let mut ba = 0;
    let mut k = 0;

    for s in S {
        let s = s.as_bytes();
        match (s[0], s[s.len() - 1]) {
            (b'B', b'A') => {
                ba += 1;
            }
            (b'B', _) => {
                b += 1;
            }
            (_, b'A') => {
                a += 1;
            }
            _ => {}
        }

        for i in 0..s.len() - 1 {
            if s[i..].starts_with(b"AB") {
                k += 1;
            }
        }
    }
    debug!((a, b, ba, k));

    if a == 0 && ba >= 1 {
        a += 1;
        ba -= 1;
    }
    if b == 0 && ba >= 1 {
        b += 1;
        ba -= 1;
    }

    // xA BA BA BA xB
    if a >= 1 && b >= 1 {
        k += ba + 1;
        a -= 1;
        b -= 1;
    }

    // xA Bx
    k += min(a, b);

    println!("{}", k)
}
