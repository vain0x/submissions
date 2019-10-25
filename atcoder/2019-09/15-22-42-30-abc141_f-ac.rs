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

// ref: https://www.geeksforgeeks.org/find-maximum-subset-xor-given-set/

fn main() {
    let N = read!(usize);
    let mut A = read![[i64]];
    A.sort();

    let mut odd = 0;
    for i in 0..N {
        odd ^= A[i];
    }

    for i in 0..N {
        A[i] &= !odd;
    }

    let mut even = 0;
    for &a in &A {
        even ^= a;
    }

    let mut index = 0;

    for i in (0..60).rev() {
        let mut max_index = index;
        let mut max_elem = std::i64::MIN;
        for j in index..N {
            if A[j] & (1 << i) != 0 && A[j] > max_elem {
                max_index = j;
                max_elem = A[j];
            }
        }

        if max_elem == std::i64::MIN {
            continue;
        }

        A.swap(index, max_index);

        max_index = index;

        for j in 0..N {
            if j != max_index && (A[j] & (1 << i) != 0) {
                A[j] ^= A[max_index];
            }
        }

        index += 1;
    }

    let mut red = 0;
    for &a in &A {
        red ^= a;
    }

    let blue = even ^ red;

    println!("{}", odd + red + blue)
}
