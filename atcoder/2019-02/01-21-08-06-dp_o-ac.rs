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

const P: i64 = 1_000_000_007;

fn main() {
    let N = read!(usize);
    let A = read![[i32]; N];

    // dp[m][s] = (男性 0..m と集合 s に含まれる女性を使ってできるペアの個数)
    // dp[m][s] = sum[f in 0..N] dp[m-1][s \ f]

    let mut dp = Vec::with_capacity(1 << N);
    dp.resize(1 << N, 0);
    dp[0] = 1;

    for m in 0..N {
        for s in 0..(1 << N) {
            let mut pop_count = 0;
            for i in 0..N {
                if s & (1 << i) != 0 {
                    pop_count += 1;
                }
            }
            if pop_count != m {
                continue;
            }

            for f in 0..N {
                if (s & (1 << f)) != 0 {
                    continue;
                }
                if A[m][f] == 0 {
                    continue;
                }

                dp[s | (1 << f)] += dp[s];
                dp[s | (1 << f)] %= P;
            }
        }
    }

    println!("{}", dp[(1 << N) - 1])
}
