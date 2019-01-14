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

const M: i64 = std::i64::MAX;

fn main() {
    let (N, W) = read!(usize, i64);
    let T = read![i64, usize; N];
    let V = T.iter().map(|&(_, v)| v).sum::<usize>();

    // dp[i][v] = w : 品物 0..i の組み合わせで、価値の合計が v のとき、最小の重み w
    let mut dp = vec![vec![M; V + 1]; N + 1];
    dp[0][0] = 0;

    for i in 0..N {
        let (dw, dv) = T[i];

        for v in 0..V + 1 {
            dp[i + 1][v] = min(dp[i + 1][v], dp[i][v]);

            if dp[i][v] != M {
                let v2 = v + dv;
                if v2 <= V {
                    dp[i + 1][v2] = min(dp[i + 1][v2], dp[i][v] + dw);
                }
            }
        }
    }

    debug!(dp);

    let m = (0..V + 1)
        .rev()
        .filter_map(|v| {
            let w = dp[N][v];
            if w <= W {
                Some(v)
            } else {
                None
            }
        })
        .next()
        .unwrap();

    println!("{}", m)
}
