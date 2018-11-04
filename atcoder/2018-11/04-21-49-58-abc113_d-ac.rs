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
    // dp[y][x][z][b] :
    // 上から y cm 未満の横棒と、左から x cm 未満の高さ y cm の横棒について、引くか引かないか決まった
    // 高さ y cm において z にいる
    // b = 1 <==> (y, x) の横棒を実際に引いた

    let (H, W, K) = read!(usize, usize, usize);

    let mut dp = vec![vec![vec![vec![0_i64; 2]; W]; W]; H + 1];
    dp[0][0][0][0] = 1;

    for y in 0..H {
        for x in 0..W - 1 {
            for z in 0..W {
                let zz = if z == x {
                    x + 1
                } else if z == x + 1 {
                    x
                } else {
                    z
                };
                dp[y][x + 1][zz][1] += dp[y][x][z][0];
                dp[y][x + 1][zz][1] %= P;

                dp[y][x + 1][z][0] += dp[y][x][z][0];
                dp[y][x + 1][z][0] %= P;
                dp[y][x + 1][z][0] += dp[y][x][z][1];
                dp[y][x + 1][z][0] %= P;
            }
        }

        for z in 0..W {
            dp[y + 1][0][z][0] += dp[y][W - 1][z][0];
            dp[y + 1][0][z][0] %= P;
            dp[y + 1][0][z][0] += dp[y][W - 1][z][1];
            dp[y + 1][0][z][0] %= P;
        }
    }

    // debug!({
    //     for y in 0..H + 1 {
    //         debug!(dp[y])
    //     }
    // });

    println!("{}", dp[H][0][K - 1][0]);
}
