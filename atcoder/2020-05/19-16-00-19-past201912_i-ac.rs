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

const INF: i64 = 1_000_000_000_000_000;

fn main() {
    let (N, M) = read!(usize, usize);

    let mut dp = vec![vec![INF; 1 << N]; M + 1];
    let mut ok = vec![vec![false; 1 << N]; M + 1];
    dp[0][0] = 0;
    ok[0][0] = true;

    for i in 0..M {
        let (S, C) = read!(String, i64);

        let t = S
            .chars()
            .enumerate()
            .map(|(i, c)| if c == 'Y' { 1 << i } else { 0 })
            .sum::<usize>();

        for u in 0..1 << N {
            if ok[i][u] {
                ok[i + 1][u] = true;
                dp[i + 1][u] = min(dp[i + 1][u], dp[i][u]);

                ok[i + 1][u | t] = true;
                dp[i + 1][u | t] = min(dp[i + 1][u | t], dp[i][u] + C);
            }
        }
    }

    let t = (1 << N) - 1;
    if ok[M][t] {
        println!("{}", dp[M][t]);
    } else {
        println!("-1");
    }
}
