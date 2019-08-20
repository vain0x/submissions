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

a
aaa
//=> 3

contest
contest
//=> 7

*/

fn char_to_index(c: char) -> usize {
    (c as u8 - b'a') as usize
}

fn solve_fast(s: &[char], t: &[char]) -> Option<usize> {
    let sn = s.len();

    // s を2個つなげたもの
    let w = s.iter().chain(s).cloned().collect::<Vec<_>>();

    // next[ci][si]:
    // w の位置 si (< sn) から見て次に文字 ci が出現する位置
    let mut next = vec![vec![None; sn]; 26];

    for ci in 0..26 {
        // w において最後に文字 ci が出現した位置
        let mut last = None;

        for wi in (0..w.len()).rev() {
            if char_to_index(w[wi]) == ci {
                last = Some(wi);
            }

            next[ci][wi % sn] = last;
        }
    }

    let mut i = 0;
    for &c in t {
        let si = i % sn;
        let ni = match next[char_to_index(c)][si] {
            None => return None,
            Some(ni) => ni,
        };
        i += ni - si + 1;
    }
    Some(i)
}

fn main() {
    let s = rl().chars().collect::<Vec<_>>();
    let t = rl().chars().collect::<Vec<_>>();

    if let Some(i) = solve_fast(&s, &t) {
        println!("{}", i)
    } else {
        println!("-1")
    }
}
