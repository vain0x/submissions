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
        X[a] += 1;
    }
    for b in B {
        Y[b] += 1;
    }

    let mut ax = 0;
    let mut bx = 0;

    let mut s = 1_i64;
    for x in (1..N * M + 1).rev() {
        if X[x] >= 2 || Y[x] >= 2 {
            s = 0;
            break;
        }

        if X[x] + Y[x] == 2 {
            ax += 1;
            bx += 1;
            continue;
        }

        if X[x] == 1 {
            s *= bx;
            s %= P;
            ax += 1;
            continue;
        }

        if Y[x] == 1 {
            s *= ax;
            s %= P;
            bx += 1;
            continue;
        }

        s *= ax * bx - (N * M - x) as i64;
        s %= P;
    }

    println!("{}", s)
}
