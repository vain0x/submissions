//! Framework <https://github.com/vain0x/procon>

#![allow(non_snake_case)]
#![allow(unused_imports)]

use std::cmp::{max, min, Ordering};
use std::collections::*;
use std::fmt::{Debug, Display, Formatter, Write as FmtWrite};
use std::io::{stderr, stdin, BufRead, Write};

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

    #[allow(deprecated)]
    buf.trim_right().to_owned()
}

// -----------------------------------------------
// Solution
// -----------------------------------------------

const INF: i64 = 1_000_000_000;

fn main() {
    let N = read!(usize);

    let mut x = vec![vec![]; N];

    for i in 0..N - 1 {
        let a = read![[i64]];
        x[i].resize(i + 1, -INF);
        x[i].extend(a);
    }
    x[N - 1].resize(N, -INF);

    let m = 3_usize.pow(N as u32);
    let mut g = vec![0; N];
    let mut best = -INF;

    for s in 0..m {
        let mut t = s;
        for i in 0..N {
            g[i] = t % 3;
            t /= 3;
        }

        let mut score = 0;
        for i in 0..N {
            for j in i + 1..N {
                if g[i] == g[j] {
                    score += x[i][j];
                }
            }
        }

        best = max(best, score);
    }

    println!("{}", best)
}
