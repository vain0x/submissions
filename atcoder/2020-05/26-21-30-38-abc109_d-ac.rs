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
    let (H, W) = read!(usize, usize);
    let board = read![[usize]; H];

    let xs = (0..W).collect::<Vec<_>>();
    let xs_rev = (0..W).rev().collect::<Vec<_>>();

    let mut ops = vec![];
    let mut prev = None;

    for y in 0..H {
        let range = if y % 2 == 0 { &xs } else { &xs_rev };

        for &x in range {
            let mut coin = board[y][x];

            if let Some((y1, x1)) = prev {
                ops.push((y1, x1, y, x));
                coin += 1;
            }

            prev = if coin % 2 != 0 { Some((y, x)) } else { None };
        }
    }

    #[cfg(debug_assertions)]
    {
        let mut board = board;
        for &(y1, x1, y2, x2) in &ops {
            board[y1][x1] -= 1;
            board[y2][x2] += 1;

            let d = (y1 as isize - y2 as isize).abs() + (x1 as isize - x2 as isize).abs();
            assert_eq!(d, 1);
        }

        let mut count = 0;
        for y in 0..H {
            for x in 0..W {
                if board[y][x] % 2 == 0 {
                    count += 1;
                }
            }
        }

        let mut expected = H * W;
        if prev.is_some() {
            expected -= 1;
        }

        debug_assert_eq!(count, expected);
    }

    println!("{}", ops.len());
    for (y1, x1, y2, x2) in ops {
        println!("{} {} {} {}", y1 + 1, x1 + 1, y2 + 1, x2 + 1);
    }
}
