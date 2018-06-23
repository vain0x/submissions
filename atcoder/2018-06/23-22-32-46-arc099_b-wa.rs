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

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Rev<T>(pub T);

impl<T: PartialOrd> PartialOrd for Rev<T> {
    fn partial_cmp(&self, other: &Rev<T>) -> Option<Ordering> {
        other.0.partial_cmp(&self.0)
    }
}

impl<T: Ord> Ord for Rev<T> {
    fn cmp(&self, other: &Rev<T>) -> Ordering {
        other.0.cmp(&self.0)
    }
}
#[derive(PartialEq)]
struct Float(pub f64);

impl PartialOrd for Float {
    fn partial_cmp(&self, other: &Float) -> Option<Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

impl Eq for Float {}

impl Ord for Float {
    fn cmp(&self, other: &Float) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

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
        let (ref mut out, K, ref mut k, ref mut pn, ref mut pr) = $context;
        let n = $n;
        let r = R(n);
        out($n as i64);
        *k += 1;
        if n >= 1_000_000_000_000_000_i64 || *k >= K {
            debug!(n, *k);
            return;
        }
        *pn = n;
        *pr = r;
    }};
}

fn gen<F: FnMut(i64) -> ()>(K: usize, out: F) {
    let mut context = (out, K, 0, 0, 0.0);

    // 1..100
    {
        for i in 1..10 {
            y!(context, i);
        }

        for i in 1..10 {
            y!(context, i * 10 + 9);
        }
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

fn check(A: &[i64]) {
    let mut result = "ok";
    let mut pn = 0;
    let mut pr = 0.0;
    for &n in A {
        let r = R(n);

        if pr > r {
            println!("n={} r={} pn={} pr={}", n, r, pn, pr);
            result = "bad";
        }

        pn = n;
        pr = r;
    }

    println!("{}", result);
}

fn enu() -> Vec<i64> {
    let mut v = Vec::new();

    {
        for i in 1..10 {
            v.push(i);
        }

        for i in 1..10 {
            v.push(i * 10 + 9);
        }
    }

    let mut b = 100;
    loop {
        for i in 1..100 {
            let n = i * b + (b - 1);

            if n > 1_000_000_000_000_000_i64 {
                let mut v = v.into_iter().map(|n| (Rev(Float(R(n))), Rev(n))).vec();
                let mut heap = std::collections::BinaryHeap::from(v);

                let mut w = Vec::new();
                let mut pr = 0.0;
                let mut pn = 0;
                while let Some((Rev(Float(r)), Rev(n))) = heap.pop() {
                    if n < pn || r < pr {
                        continue;
                    }
                    w.push(n);
                    pn = n;
                    pr = r;
                }
                return w;
            }

            v.push(n);
        }

        b *= 100;
    }
}

pub fn main() {
    let K = read!(usize);
    let A = enu().into_iter().take(K).vec();

    #[cfg(debug_assertions)]
    check(&A);

    let stdout = stdout();
    let mut out = stdout.lock();
    {
        let mut put = |n: i64| out.write_fmt(format_args!("{}\n", n)).unwrap();

        // gen(K, put);
        for &n in &A {
            put(n);
        }
    }
    out.flush().unwrap();

    //list();
}
