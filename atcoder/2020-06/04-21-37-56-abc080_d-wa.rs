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
    let (N, C) = read!(usize, usize);
    let mut T = read![usize, usize, usize; N];

    T.sort_by_key(|&(s, t, c)| (c, s, t));

    let mut i = 0;
    while i + 1 < N {
        let (s1, t1, c1) = T[i];
        let mut j = i + 1;

        while j < N {
            let (s2, t2, c2) = T[j];
            if c1 != c2 || t1 != s2 {
                break;
            }

            T[i] = (s1, t2, c1);
            T[j] = (0, 0, std::usize::MAX);
            j += 1;
        }

        i = j;
    }
    T.retain(|&(_, _, c)| c != std::usize::MAX);
    let N = ();

    let mut events = vec![];
    for &(s, t, c) in &T {
        events.push((s, 0, c));
        events.push((t, 1, c));
    }
    events.sort();

    debug!(T, events);

    let mut alloc = 0;
    let mut free = 0;
    for (_, ev, _) in events {
        if ev == 0 {
            // 録画開始
            if free >= 1 {
                free -= 1;
            } else {
                alloc += 1;
            }
        } else {
            // 録画終了
            free += 1;
        }
    }

    println!("{}", alloc)
}
