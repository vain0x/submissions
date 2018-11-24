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

const P: i64 = 1_000_000_007;
const K: i64 = 10;

fn main() {
    let N = read!(usize);

    // pk[x] = x^K
    let mut pk = vec![1; N + 1];
    for i in 1..N + 1 {
        let mut s = 1_i64;
        for _ in 0..K {
            s = (s * i as i64) % P;
        }
        pk[i] = s;
    }

    // f[x] = 最大値がちょうどxである長さKの順列の数
    let f = (0..N + 1)
        .map(|x| {
            if x <= 1 {
                pk[x]
            } else {
                (pk[x] + (P - pk[x - 1])) % P
            }
        })
        .collect::<Vec<_>>();

    let mut s = 0_i64;
    for p in 1..N + 1 {
        let q = N / p;

        debug!((p, q, f[p], pk[q]));
        s = (s + f[p] * pk[q] % P) % P;
    }
    println!("{}", s)
}
