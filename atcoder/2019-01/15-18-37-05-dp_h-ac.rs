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
use std::fmt::{Debug, Formatter, Write as FmtWrite};
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
    let (H, W) = read!(usize, usize);
    let A = read![String; H]
        .into_iter()
        .map(|s| s.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();

    let mut dp = vec![vec![0; W]; H];
    dp[0][0] = 1_i64;

    for y in 0..H {
        for x in 0..W {
            if y + 1 < H && A[y + 1][x] == '.' {
                dp[y + 1][x] += dp[y][x];
                dp[y + 1][x] %= P;
            }

            if x + 1 < W && A[y][x + 1] == '.' {
                dp[y][x + 1] += dp[y][x];
                dp[y][x + 1] %= P;
            }
        }
    }

    println!("{}", dp[H - 1][W - 1]);
}
