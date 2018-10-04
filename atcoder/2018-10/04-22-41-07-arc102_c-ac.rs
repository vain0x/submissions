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

#[cfg(debug_assertions)]
include!{"./procon/debug.rs"}

#[cfg(not(debug_assertions))]
macro_rules! debug {
    ($($arg:expr),*) => {};
}

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

// -----------------------------------------------
// Solution
// -----------------------------------------------

// prime
const P: i64 = 998244353;

pub fn pow(x: i64, n: i64) -> i64 {
    let (mut x, mut y, mut n) = (x % P, 1, n);
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

fn main() {
    // >> N + K
    let M = 100_000;

    // 階乗
    let mut fact = vec![1_i64; M + 1];
    for i in 1..M {
        fact[i + 1] = fact[i] * (i + 1) as i64 % P;
    }

    // (x!)^-1
    // フェルマーの小定理を使う
    let fact_inv = (0..M + 1).map(|i| pow(fact[i], P - 2)).vec();

    // 二項係数
    let combo = |p: i64, q: i64| {
        if 0 <= q && q <= p {
            fact[p as usize] * fact_inv[q as usize] % P * fact_inv[(p - q) as usize] % P
        } else {
            0
        }
    };

    // H(p, q)
    let hcombo = |p: i64, q: i64| {
        if p == 0 && q == 0 {
            1
        } else {
            combo(p + q - 1, q)
        }
    };

    let pow2 = (0..M + 1).map(|i| pow(2, i as i64)).vec();

    let (K, N) = read!(i64, i64);

    for t in 2..2 * K + 1 {
        let p = (1..K + 1).filter(|&s| s < t - s && t - s <= K).count() as i64;

        let mut z = 0_i64;

        for q in 0..p + 1 {
            // C(p, q)
            let mut w = combo(p, q);

            // 2^q
            w *= pow2[q as usize];
            w %= P;

            if t % 2 == 0 {
                // t/2 が1つ出る場合と全く出ない場合の両方を計算する。
                w *= (hcombo(q + K - 2 * p - 1, N - q) + hcombo(q + K - 2 * p - 1, N - q - 1)) % P;
                w %= P;
            } else {
                w *= hcombo(q + K - 2 * p, N - q);
                w %= P;
            }

            z += w;
            z %= P;
        }

        println!("{}", z);
    }
}
