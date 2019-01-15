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

fn main() {
    let (N, M) = read!(usize, usize);
    let A = read![[usize]];
    let B = read![[usize]];

    let mut X = vec![0; N * M + 1];
    let mut Y = vec![0; N * M + 1];

    for a in A {
        X[a] = 1;
    }
    for b in B {
        Y[b] = 1;
    }

    let mut SX = vec![0; N * M + 2];
    let mut SY = vec![0; N * M + 2];
    for i in (1..N * M + 1).rev() {
        SX[i - 1] += SX[i] + X[i];
        SY[i - 1] += SY[i] + Y[i];
    }

    debug!(X, Y, SX, SY);

    let mut s = 1_i64;
    for x in (1..N * M + 1).rev() {
        if X[x] >= 2 || Y[x] >= 2 {
            s = 0;
            break;
        }

        if X[x] + Y[x] == 2 {
            continue;
        }

        if X[x] == 1 {
            s *= SY[x];
            s %= P;
            continue;
        }

        if Y[x] == 1 {
            s *= SX[x];
            s %= P;
            continue;
        }

        let mut t = SX[x] * SY[x];
        t = t - min(t, (N * M - x) as i64);

        s *= t;
        s %= P;
    }

    println!("{}", s)
}
