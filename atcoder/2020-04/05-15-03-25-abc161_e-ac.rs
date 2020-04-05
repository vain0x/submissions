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

5 2 2
oxoxo
//=> 1, 5

*/

fn main() {
    let (N, K, C) = read!(usize, usize, usize);
    let S = rl().chars().collect::<Vec<_>>();

    // left[i] = (i 日目より前に働く日数の最大値)
    let mut left = vec![0_usize; N + 1];

    // right[i] = (i 日目以降に働く日数の最大値)
    let mut right = vec![0_usize; N + 1];

    for i in 0..N {
        if S[i] == 'o' {
            left[i + 1] = left[(i + 1).saturating_sub(C + 1)] + 1;
        }

        left[i + 1] = max(left[i + 1], left[i]);
    }

    for i in (0..N).rev() {
        if S[i] == 'o' {
            right[i] = 1;
            if i + C + 1 < N {
                right[i] += right[i + C + 1];
            }
        }

        right[i] = max(right[i], right[i + 1]);
    }

    // let iota = (0..N).collect::<Vec<_>>();
    // debug!(iota, left, right, first);

    for i in 0..N {
        let n = left[i] + right[i + 1];
        if n < K {
            println!("{}", i + 1);
        }
    }
}
