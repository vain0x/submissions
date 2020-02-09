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
    let N = rl().chars().collect::<Vec<char>>();
    let K = read!(usize);

    let h = N.len();

    let d1 = (N.get(0).cloned().unwrap_or('0') as u8 - b'0') as usize;
    let d2 = (N.get(1).cloned().unwrap_or('0') as u8 - b'0') as usize;
    let d3 = (N.get(2).cloned().unwrap_or('0') as u8 - b'0') as usize;
    let mut s = 0;

    debug!(h, d1, d2, d3);

    // = : N の同じ桁と同じ数字
    // * : N の同じ桁より小さい、1以上の数字
    // 9900 (9がK個のN桁) : N桁の中に1以上の数字がちょうどK個含まれる

    match K {
        1 => {
            // =00
            // *00
            s += d1;

            // 090
            s += (h - 1) * 9;
        }
        2 if h >= 2 => {
            // ==00
            // =*00
            s += d2;

            // =090
            s += (h - 2) * 9;

            // *900
            s += (d1 - 1) * (h - 1) * 9;

            // 0990
            s += ((h - 1) * (h - 2) / 2) * 9 * 9;
        }
        3 if h >= 3 => {
            // ==:
            // ===000
            // ==*000
            s += d3;

            // ==0900
            s += (h - 3) * 9;

            // =*9000
            s += (d2 - 1) * (h - 2) * 9;

            // =09900
            s += (h - 2) * (h - 3) / 2 * 9 * 9;

            // *99000
            s += (d1 - 1) * (h - 1) * (h - 2) / 2 * 9 * 9;

            // 099900
            s += (h - 1) * (h - 2) * (h - 3) / 6 * 9 * 9 * 9;
        }
        _ => {}
    }

    println!("{}", s)
}
