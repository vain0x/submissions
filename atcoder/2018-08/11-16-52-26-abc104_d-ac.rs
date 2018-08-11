#![allow(unused_imports)]
#![allow(non_snake_case)]

use std::cell::RefCell;
use std::cmp::{max, min, Ordering};
use std::collections::*;
use std::fmt::{Debug, Formatter};
use std::io::*;
use std::mem::{replace, swap};
use std::ops::*;
use std::rc::Rc;

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
            stderr().write_fmt(format_args!("{:?}\n", entries)).unwrap();
        }
    };
}

#[allow(dead_code)]
fn rl() -> String {
    let mut buf = String::new();
    stdin().read_line(&mut buf).unwrap();
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

pub fn main() {
    let S = rl().chars().vec();
    let N = S.len();

    let P = 1_000_000_007_i64;
    let ABC = "ABC".chars().vec();

    // automaton DP?
    // dp[r][i] = t mod P
    // i : S の範囲 S[..i]
    // r : 固定した位置の個数
    // t : 範囲 S[..i] において決められた r 個の位置の文字がそれぞれ A, B, C であるような文字列の個数 (mod P)
    let mut dp = vec![vec![0; N + 1]; 3 + 1];
    dp[0][0] = 1_i64;

    for i in 0..N {
        // 位置 i をABC数に含めないケースの状態遷移
        let f = if S[i] == '?' { 3 } else { 1 };
        for r in 0..4 {
            dp[r][i + 1] = (dp[r][i + 1] + f * dp[r][i] % P) % P;
        }

        // 位置 i をABC数に含めるケースの状態遷移
        for r in 0..3 {
            if S[i] == ABC[r] || S[i] == '?' {
                dp[r + 1][i + 1] = (dp[r + 1][i + 1] + dp[r][i]) % P;
            }
        }
    }

    println!("{}", dp[3][N]);
}
