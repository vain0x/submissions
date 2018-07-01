#![allow(unused_imports)]
#![allow(non_snake_case)]

use std::cmp::{max, min, Ordering};
use std::collections::*;
use std::fmt::{Debug, Formatter};
use std::io::*;
use std::ops::*;
use std::*;

// -----------------------------------------------
// Framework
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
        {
            let entries = [$(&stringify!([$arg]:), &$arg as &Debug),*];
            stderr().write_fmt(format_args!("{:#?}\n", entries)).unwrap();
        }
    };
}

#[allow(dead_code)]
fn rl() -> String {
    let mut buf = String::new();
    io::stdin().read_line(&mut buf).unwrap();
    buf.trim_right().to_owned()
}

trait IteratorExt: Iterator + Sized {
    fn vec(self) -> Vec<Self::Item> {
        self.collect()
    }
}

impl<T: Iterator> IteratorExt for T {}

// -----------------------------------------------
// Solution
// -----------------------------------------------

fn trial(N: usize, A: &Vec<i64>) {
    let mut sol = (std::i64::MAX, None);

    for i in 1..N {
        for j in i + 1..N {
            for k in j + 1..N {
                let p: i64 = A[0..i].iter().sum();
                let q: i64 = A[i..j].iter().sum();
                let r: i64 = A[j..k].iter().sum();
                let s: i64 = A[k..N].iter().sum();
                let ma = max(max(max(p, q), r), s);
                let mi = min(min(min(p, q), r), s);
                let d = ma - mi;
                if d < sol.0 {
                    sol = (d, Some((i, j, k)));
                }
            }
        }
    }

    debug!(sol);
}

fn solve(A: &[i64]) -> (usize, i64, i64) {
    let N = A.len();

    let mut min_d = std::i64::MAX;
    let mut min_s = (0, 0, 0);

    let mut D = vec![0; N + 1];
    for i in 0..N {
        D[i + 1] += D[i] + A[i];
    }

    for i in 1..N {
        let p = D[i];
        let q = D[N] - p;

        let d = (p - q).abs();
        if d < min_d {
            min_d = d;
            min_s = (i, max(p, q), min(p, q));
        }
    }

    min_s
}

pub fn main() {
    let N = read!(usize);
    let A = read![[i64]];

    let mut min_d = std::i64::MAX;

    for j in 2..N - 1 {
        let l = solve(&A[0..j]);
        let r = solve(&A[j..N]);

        let ma = max(l.1, r.1);
        let mi = min(l.2, r.2);
        let d = ma - mi;

        if d < min_d {
            min_d = d;
        }
    }

    println!("{}", min_d);

    return;
}
