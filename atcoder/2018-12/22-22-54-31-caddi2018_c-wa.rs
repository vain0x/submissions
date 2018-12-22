// -----------------------------------------------
// Framework <https://github.com/vain0x/procon>
//
// See the bottom of file for solution.
// -----------------------------------------------

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

#[allow(dead_code)]
fn rl() -> String {
    let mut buf = String::new();
    stdin().read_line(&mut buf).unwrap();
    buf.trim_right().to_owned()
}

// -----------------------------------------------
// Solution
// -----------------------------------------------

fn do_neg(A: &[i64]) -> usize {
    let mut A = Vec::from(A);
    let mut k = 0;

    for a in A.iter_mut() {
        if *a > 0 {
            *a *= -2;
            k += 1;
        }
    }

    k
}

fn do_pos(A: &[i64]) -> usize {
    let mut prev = 0;
    let mut k = 0;
    for mut a in A.iter().cloned() {
        if a >= prev {
            prev = a;
            continue;
        }

        let mut h = 0;
        while a < prev {
            h += 2;
            a *= 4;
        }

        k += h;
        prev = a;
    }
    k
}

fn main() {
    read!(usize);
    let A = read![[i64]];
    let N = A.len();

    let mut min_k = std::usize::MAX;
    for i in 0..N + 1 {
        let mut k = 0;
        k += do_neg(&A[..i]);
        k += do_pos(&A[i..]);
        min_k = min(min_k, k);
    }

    println!("{}", min_k)
}
