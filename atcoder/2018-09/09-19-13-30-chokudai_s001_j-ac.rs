#![allow(unused_imports)]
#![allow(non_snake_case)]

use std::cell::RefCell;
use std::cmp::{max, min, Ordering};
use std::collections::*;
use std::fmt::{Debug, Formatter, Write as FmtWrite};
use std::io::{stderr, stdin, BufRead, Write};
use std::mem::{replace, swap};
use std::ops::*;
use std::rc::Rc;

// -----------------------------------------------
// Framework <https://github.com/vain0x/procon>
// -----------------------------------------------

#[allow(unused_macros)]
macro_rules! read {
    ([$t:ty] ; $n:expr) =>
        ((0..$n).map(|_| read!([$t])).collect::<Vec<_>>());
    ($($t:ty),+ ; $n:expr) =>
        ((0..$n).map(|_| read!($($t),+)).collect::<Vec<_>>());
    ([$t:ty]) =>
        (rl().split_whitespace().map(|w| w.parse().unwrap()).collect::<Vec<$t>>());
    ($t:ty) =>
        (rl().parse::<$t>().unwrap());
    ($($t:ty),*) => {{
        let buf = rl();
        let mut w = buf.split_whitespace();
        ($(w.next().unwrap().parse::<$t>().unwrap()),*)
    }};
}

#[allow(unused_macros)]
macro_rules! debug {
    ($($arg:expr),*) => {
        #[cfg(debug_assertions)]
        $(writeln!(stderr(), "{} = {:?}", stringify!($arg), $arg).unwrap());*
    };
}

#[allow(dead_code)]
fn rl() -> String {
    let mut buf = String::new();
    stdin().read_line(&mut buf).unwrap();
    buf.trim_right().to_owned()
}

trait IteratorExt: Iterator + Sized {
    fn vec(self) -> Vec<Self::Item> {
        self.collect()
    }
}

impl<T: Iterator> IteratorExt for T {}

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

// -----------------------------------------------
// Solution
// -----------------------------------------------

fn main() {
    rl();
    let A = read![[usize]];
    let N = A.len();

    let M = A.iter().cloned().max().unwrap() + 1;

    let mut bit = bit_new(M);
    let mut k = 0_i64;
    for i in 0..N {
        // debug!(i, A[i], bit_sum(&bit, A[i], M));

        k += bit_sum(&bit, A[i], M);
        bit_add(&mut bit, A[i], 1);
    }

    println!("{}", k)
}
