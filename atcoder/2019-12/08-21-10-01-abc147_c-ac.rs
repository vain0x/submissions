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

fn pop_count(x: usize) -> usize {
    let mut n = 0;
    for i in 0..32 {
        if (x & (1 << i)) != 0 {
            n += 1;
        }
    }
    n
}

fn main() {
    let N = read!(usize);
    let mut T = vec![vec![]; N];

    for i in 0..N {
        let A = read!(usize);
        for (x, y) in read![usize, i64; A] {
            T[i].push((x - 1, y != 0));
        }
    }

    let mut m = 0;

    's: for s in 1..1 << N {
        let k = pop_count(s);
        if k < m {
            continue;
        }

        for i in 0..N {
            if (s & (1 << i)) == 0 {
                // 不親切な人の主張は何であれ矛盾しない。
                continue;
            }

            for &(x, y) in &T[i] {
                // x は正直者？
                let honest = (s & (1 << x)) != 0;

                if (honest && !y) || (!honest && y) {
                    // 矛盾
                    continue 's;
                }
            }
        }

        m = k;
    }

    println!("{}", m)
}
