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

// 1-indexed. [0] is unused.
type BIT = Vec<i64>;

pub fn bit_new(len: usize) -> BIT {
    vec![0; len + 1]
}

pub fn bit_len(bit: &BIT) -> usize {
    bit.len() - 1
}

/// Increments an element.
pub fn bit_add(bit: &mut BIT, index: usize, value: i64) {
    let mut j = index + 1;
    while j <= bit_len(bit) {
        bit[j] += value;
        j += rightmost_bit(j);
    }
}

/// Gets sum of items in range 0..right.
pub fn bit_acc(bit: &BIT, right: usize) -> i64 {
    let mut acc = 0;
    let mut j = min(right, bit_len(bit));
    while j > 0 {
        acc += bit[j];
        j -= rightmost_bit(j);
    }
    acc
}

/// Gets sum of items in range left..right.
pub fn bit_sum(bit: &BIT, left: usize, right: usize) -> i64 {
    bit_acc(bit, right) - bit_acc(bit, min(left, right))
}

fn rightmost_bit(n: usize) -> usize {
    let s = n as isize;
    (s & -s) as usize
}

fn main() {
    let N = read!(usize);
    let mut S = rl().chars().collect::<Vec<_>>();
    let Q = read!(usize);

    let mut bits = vec![bit_new(N); 26];

    for i in 0..N {
        let ci = (S[i] as u8 - b'a') as usize;
        bit_add(&mut bits[ci], i, 1);
    }

    for _ in 0..Q {
        let line = rl();
        let mut q = line.split_whitespace();
        let t = q.next().unwrap().parse::<i32>().unwrap();

        if t == 1 {
            let mut i = q.next().unwrap().parse::<usize>().unwrap();
            let c = q.next().unwrap().chars().next().unwrap();
            i -= 1;

            let old_ci = (S[i] as u8 - b'a') as usize;
            let new_ci = (c as u8 - b'a') as usize;
            if old_ci == new_ci {
                continue;
            }

            bit_add(&mut bits[old_ci], i, -1);
            bit_add(&mut bits[new_ci], i, 1);
            S[i] = c;
        } else {
            let mut l = q.next().unwrap().parse::<usize>().unwrap();
            let r = q.next().unwrap().parse::<usize>().unwrap();
            l -= 1;

            let mut n = 0;
            for ci in 0..26 {
                if bit_sum(&bits[ci], l, r) != 0 {
                    n += 1;
                }
            }

            println!("{}", n)
        }
    }
}
