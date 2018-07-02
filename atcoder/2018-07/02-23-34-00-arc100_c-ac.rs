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

// -----------------------------------------------
// Solution
// -----------------------------------------------

fn exp() {
    let N = 4;
    let P = 1 << N;

    let mut prev = HashSet::new();

    for K in 1..P {
        let mut S = Vec::new();

        for i in 0..P {
            for j in i + 1..P {
                let s = i | j;
                if s > K {
                    continue;
                }

                S.push((i, j));
            }
        }

        S.sort();
        println!(
            "K={}\n{}\n",
            K,
            S.iter()
                .cloned()
                .filter(|x| !prev.contains(x))
                .map(|(i, j): (usize, usize)| format!("({}, {})", i, j))
                .vec()
                .join(" ")
        );

        prev = S.into_iter().collect();
    }
}

fn core() {
    let stdout = std::io::stdout();
    let mut stdout = stdout.lock();
    let mut p = move |n: i64| stdout.write_fmt(format_args!("{}\n", n)).unwrap();

    let N = read!(usize);
    let A = read![[i64]];

    let P = 1 << N;

    // T[n] = max A_i for i in n
    let mut T = vec![0; P];

    T[0] = A[0];
    T[1] = max(A[0], A[1]);
    T[2] = max(A[0], A[2]);

    let mut prev = A[0] + A[1];

    // K = 1
    p(prev);

    for K in 2..P {
        let mut ma = prev;

        let mut h = 1;
        while (1 << h) <= K {
            h += 1;
        }
        assert!((1 << h) >= K);

        let mut mask = 0;
        for i in (0..h).rev() {
            mask = mask | (1 << i);
            let s = K & !mask;

            // A_i + A_K (i in s)
            ma = max(ma, T[s] + A[K]);

            {
                let i = K & !mask;
                let j = K & mask;
                ma = max(ma, T[i] + T[j]);
            }
        }

        p(ma);
        prev = ma;
    }
}

pub fn main() {
    let stdout = std::io::stdout();
    let mut stdout = stdout.lock();
    let mut p = move |n: i64| stdout.write_fmt(format_args!("{}\n", n)).unwrap();

    let N = read!(usize);
    let P = 1 << N;
    let A = read![[i64]];

    let mut dp = vec![vec![]; P];
    dp[0] = vec![Rev((A[0], 0))];

    for s in 0..P {
        dp[s].push(Rev((A[s], s)));
        dp[s].sort();
        dp[s].dedup();
        while dp[s].len() > 2 {
            dp[s].pop();
        }

        let dps = dp[s].clone();

        for h in 0..N {
            let t = s | (1 << h);

            for x in dps.iter().cloned() {
                dp[t].push(x);
            }
            dp[t].sort();
            dp[t].dedup();
            while dp[t].len() > 2 {
                dp[t].pop();
            }
        }
    }

    let mut ma = 0;
    for k in 1..P {
        let mut heap = std::mem::replace(&mut dp[k], Vec::new());
        heap.sort();
        let Rev((a, _)) = heap.pop().unwrap();
        let Rev((b, _)) = heap.pop().unwrap();
        ma = max(ma, a + b);
        p(ma);
    }

    return;
}
