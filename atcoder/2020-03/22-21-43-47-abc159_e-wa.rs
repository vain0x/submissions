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
    let (H, W, K) = read!(usize, usize, usize);
    let T = read![String; H];

    let board = T
        .into_iter()
        .map(|row| row.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();

    let mut rev = vec![0; H];
    let mut whites = vec![0; H];
    let mut inc = vec![0; H];
    let mut min_k = std::usize::MAX;
    let mut k;
    let mut impossible;

    for s in 0..1 << (H - 1) {
        impossible = false;
        k = 0;

        let bn = {
            let mut bi = 0;
            for y in 0..H {
                rev[y] = bi;

                if (s & (1 << y)) != 0 {
                    bi += 1;
                    k += 1;
                }
            }
            bi + 1
        };

        debug!((s, &rev, bn));

        if k >= min_k {
            continue;
        }

        for bi in 0..bn {
            whites[bi] = 0;
        }

        for x in 0..W {
            debug_assert!((0..bn).all(|i| inc[i] == 0));

            for y in 0..H {
                if board[y][x] == '1' {
                    inc[rev[y]] += 1_usize;
                }
            }

            debug!((x, &whites, &inc));

            if (0..bn).any(|i| inc[i] > K) {
                impossible = true;
                break;
            }

            if (0..bn).any(|i| whites[i] + inc[i] > K) {
                k += 1;

                for bi in 0..bn {
                    whites[bi] = inc[bi];
                    inc[bi] = 0;
                }
                continue;
            }

            for bi in 0..bn {
                whites[bi] += inc[bi];
                inc[bi] = 0;
            }
        }

        if impossible {
            debug!("impossible");
            continue;
        }

        min_k = min(min_k, k);
    }

    println!("{}", min_k)
}
