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

// -----------------------------------------------
// Solution
// -----------------------------------------------

const P: i64 = 1_000_000_007;

fn pow(mut x: i64, mut n: i64) -> i64 {
    let mut y = 1_i64;
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
    let (N, M) = read!(usize, i64);

    let r = (M as f64).sqrt() as i64 + 2;

    let mut qs = vec![];
    {
        let mut x = M;
        for p in 2..r as i64 {
            let mut k = 0;
            while x >= p && x % p == 0 {
                x /= p;
                k += 1;
            }
            if k > 0 {
                qs.push(k);
            }
        }
        if x > 1 {
            qs.push(1);
        }
    }

    let M = qs.iter().max().cloned().unwrap_or(1) + N + 1;

    let mut fact = vec![1_i64; M];
    for i in 2..M {
        fact[i] = i as i64 % P * fact[i - 1] % P;
    }

    let inv_fact = fact.iter().map(|&x| pow(x, P - 2)).vec();

    let mut s = 1_i64;
    for q in qs {
        let c = fact[q + N - 1] * inv_fact[q] % P * inv_fact[N - 1] % P;
        s = s * c % P;
    }

    println!("{}", s);
}
