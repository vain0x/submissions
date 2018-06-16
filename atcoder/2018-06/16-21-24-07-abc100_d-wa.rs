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

type V = (i64, i64, i64);

fn dot((sa, sb, sc): V, (x, y, z): V) -> V {
    (sa * x, sb * y, sc * z)
}

fn score(S: V, T: V) -> i64 {
    sum(dot(S, T))
}

fn sum((x, y, z): V) -> i64 {
    x + y + z
}

pub fn main() {
    // (sa, sb, sc) : a,b,cにかかる係数
    // dp[v] = (a, b, c) : v個選んでいて、S dot (a, b, c) の値が最大になる選び方

    let (N, M) = read!(usize, usize);
    let t = read![i64, i64, i64; N];

    let mut max_score = 0;

    for &sa in &[1, -1] {
        for &sb in &[1, -1] {
            for &sc in &[1, -1] {
                let mut dp = vec![0; M + 1];
                let S = (sa, sb, sc);

                for i in 0..N {
                    for w in (0..M).rev() {
                        dp[w + 1] = max(dp[w + 1], dp[w] + score(S, t[i]));
                    }
                }

                max_score = max(max_score, dp[M]);
            }
        }
    }

    println!("{}", max_score);

    return;
}
