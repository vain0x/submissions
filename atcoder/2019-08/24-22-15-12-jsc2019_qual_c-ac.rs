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

const P: i64 = 1_000_000_007;

pub fn pow(x: i64, n: i64) -> i64 {
    let (mut x, mut y, mut n) = (x % P, 1_i64, n);
    while n > 0 {
        if n % 2 != 0 {
            y = (y * x) % P;
            n -= 1;
        }

        x = (x * x) % P;
        n /= 2;
    }
    y
}

fn inv(c: char) -> char {
    if c == 'W' {
        'B'
    } else {
        'W'
    }
}

fn solve(S: &[char]) -> i64 {
    let N = S.len() / 2;

    let mut shift = vec![0; 2 * N];
    let mut action = vec!['\0'; 2 * N];
    let mut s = 0_i64;
    let mut ok = true;
    for i in 0..2 * N {
        let c = if s % 2 == 0 { S[i] } else { inv(S[i]) };
        if c == 'B' {
            s += 1;
            shift[i] = s;
            action[i] = 'U';
        } else {
            shift[i] = s;
            action[i] = 'D';
            s -= 1;
            if s < 0 {
                ok = false;
                break;
            }
        }
    }
    // debug!(shift, action, ok);

    if !ok {
        return 0;
    }

    let mut count = 1_i64;
    let mut down = 0_i64;
    for i in (0..2 * N).rev() {
        if action[i] == 'D' {
            down += 1;
            continue;
        }

        if down == 0 {
            return 0;
        }
        count *= down;
        count %= P;
        down -= 1;
    }

    for i in 1..N + 1 {
        count *= i as i64;
        count %= P;
    }

    count
}

fn main() {
    read!(usize);
    let S = rl().chars().collect::<Vec<_>>();

    println!("{}", solve(&S))
}
