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

fn sq(x: usize) -> i64 {
    (x * x) as i64
}

fn main() {
    let (N, K) = read!(usize, usize);
    let mut T = read![usize, i64; N];

    T.sort_by_key(|&(_, d)| -d);

    let mut dup = vec![];
    let mut kind = BTreeSet::new();
    let mut sum = 0;

    for i in 0..K {
        let (t, d) = T[i];
        if !kind.insert(t) {
            dup.push(d);
        }
        sum += d;
    }

    debug!(sum, dup, kind);

    let mut max_score = sum + sq(kind.len());

    for i in K..N {
        let (t, d) = T[i];
        if !kind.insert(t) {
            debug!((t, d), "skip");
            continue;
        }

        if let Some(vomit) = dup.pop() {
            debug!((t, d, vomit));
            sum += d - vomit;
            max_score = max(max_score, sum + sq(kind.len()));
            debug!((sum, max_score));
        } else {
            break;
        }
    }

    println!("{}", max_score)
}
