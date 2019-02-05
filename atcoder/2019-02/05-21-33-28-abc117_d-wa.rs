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
use std::fmt::{Debug, Display, Formatter, Write as FmtWrite};
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

const B: usize = 50;

fn main() {
    let (N, K) = read!(usize, u64);
    let A = read![[u64]];

    if K == 0 {
        let Y = A.into_iter().sum::<u64>();
        println!("{}", Y);
        return;
    }

    // dp[t][u] = y
    //   x の上位 t 桁を決定した状態での最大の f(x)
    //   u : いまの x が K に一致していれば 1　、真に小さければ 0
    let mut dp = vec![vec![0; 2]; B + 1];

    let mut all_zero = true;

    for t in 0..B {
        let s = B - 1 - t;
        let k = (K >> s) & 1;

        if all_zero && k == 0 {
            continue;
        }
        all_zero = false;

        for eq in 0..2 {
            let x = if eq == 1 { k + 1 } else { 2 };

            for b in 0..x {
                let eq_next = if eq == 1 && b == k { 1 } else { 0 };
                debug!((t, eq, b, s, eq_next));

                let mut sum = 0;
                for i in 0..N {
                    sum += (A[i] & (1 << s)) ^ (b << s);
                }

                dp[t + 1][eq_next] = max(dp[t + 1][eq_next], dp[t][eq] + sum);
            }
        }
    }

    debug!(dp);

    println!("{}", max(dp[B][0], dp[B][1]))
}
