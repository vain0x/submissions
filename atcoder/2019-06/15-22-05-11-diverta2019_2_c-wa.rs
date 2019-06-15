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
    let mut A = read![[i64]];
    A.sort();

    let parity = if N % 2 == 0 || A[N / 2] > 0 { 0 } else { 1 };
    let border = N / 2 + parity;

    let mut ops = vec![];
    let mut xs = vec![];
    let mut ys = vec![];

    for i in 0..border {
        ys.push(A[i]);
    }
    for i in border..N {
        xs.push(A[i]);
    }

    while xs.len() < ys.len() {
        let x = xs.pop().unwrap();
        let y = ys.pop().unwrap();
        ops.push((x, y));
        xs.push(x - y);
    }

    while ys.len() > 0 {
        assert!(xs.len() - ys.len() <= 1);

        let x = xs.pop().unwrap();
        let y = ys.pop().unwrap();

        if xs.len() > ys.len() {
            ops.push((y, x));
            ys.push(y - x);
        } else {
            ops.push((x, y));
            xs.push(x - y);
        }
    }

    println!("{}", xs.pop().unwrap());
    for (x, y) in ops {
        println!("{} {}", x, y);
    }
}
