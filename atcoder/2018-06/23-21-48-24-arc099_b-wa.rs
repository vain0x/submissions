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

fn S(N: i64) -> i64 {
    if N < 10 {
        N
    } else {
        N % 10 + S(N / 10)
    }
}

fn R(N: i64) -> f64 {
    N as f64 / S(N) as f64
}

fn list() {
    let mut table = (1..1_000_000).map(|n| (R(n), n)).vec();
    table.sort_by(|&l, r| l.partial_cmp(r).unwrap());

    let mut prev = 0;
    let mut k = 1;
    for (r, n) in table {
        if n != 109 && n <= prev {
            continue;
        }

        println!("k={} n={} r={}", k, n, r);

        k += 1;
        prev = n;
    }
}

macro_rules! y {
    ($context:expr, $n:expr) => {{
        let (ref mut out, K, ref mut k) = $context;
        out($n as i64);
        *k += 1;
        if $n >= 1_000_000_000_000_000_i64 || *k >= K {
            return;
        }
    }};
}

fn gen<F: FnMut(i64) -> ()>(K: usize, out: F) {
    let mut context = (out, K, 0);

    for i in 1..10 {
        y!(context, i);
    }

    for i in 1..10 {
        y!(context, i * 10 + 9);
    }

    let mut b = 100;
    loop {
        for i in 1..20 {
            y!(context, i * b + (b - 1));
        }

        for i in 2..10 {
            y!(context, i * b * 10 + (b * 10 - 1));
        }

        b *= 100;
    }
}

fn check(K: usize) {
    let mut vec = Vec::new();
    gen(K, |n: i64| vec.push(n));

    let mut pn = 0;
    let mut pr = 0.0;
    for n in vec.into_iter() {
        let r = R(n);

        if pr > r {
            println!("n={} r={} pn={} pr={}", n, r, pn, pr);
        }

        pn = n;
        pr = r;
    }
}

pub fn main() {
    let K = read!(usize);

    check(K);

    let stdout = stdout();
    let mut out = stdout.lock();
    gen(K, |n: i64| out.write_fmt(format_args!("{}\n", n)).unwrap());
    out.flush().unwrap();

    //list();
}
