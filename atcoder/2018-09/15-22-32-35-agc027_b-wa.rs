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
        $(writeln!(stderr(), "{} = {:?}", stringify!($arg), $arg).unwrap());*
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

struct Calc {
    e: i64,
    X: Vec<i64>,
    D: Vec<i64>,
    dp: Vec<i64>,
}

impl Calc {
    // l 未満のゴミが拾い終わったあと、 l~r のゴミを一周で拾って、残りのゴミをすべて拾うコスト
    fn cost(&mut self, l: usize, r: usize) -> i64 {
        let mut c = 0_i64;

        if l < r {
            // 点 r-1 までいく
            c += self.X[r - 1];

            // ゴミを拾いながら 0 まで戻る
            let mut k = 0_i64;
            for i in (l..r).rev() {
                k += 1;
                c += self.e;

                let u = (k + 1) * (k + 1);
                let d = if i == l {
                    self.X[i]
                } else {
                    self.D[i]
                };
                c += d * u;
            }

            // ゴミ箱に入れる
            c += self.e;
        }

        // 残り
        c += self.calc(r);

        // debug!(("cost", l, r), c);

        c
    }

    fn calc(&mut self, l: usize) -> i64 {
        if l == self.X.len() {
            return 0;
        }
        if self.dp[l] != std::i64::MAX {
            return self.dp[l];
        }

        let mut cost = std::i64::MAX;
        for r in l + 1..self.X.len() + 1 {
            cost = min(cost, self.cost(l, r));
        }

        // let mut il = l + 1; // ok, 微分係数は負
        // let mut ir = self.X.len() + 1; // ng
        // while ir - il > 1 {
        //     let m = il + (ir - il) / 2;
        //     let d = self.cost(l, m) - self.cost(l, m - 1);
        //     if d <= 0 {
        //         il = m;
        //     } else {
        //         ir = m;
        //     }
        // }
        // let cost = self.cost(l, il);

        self.dp[l] = cost;
        cost
    }
}

fn main() {
    let (_, e) = read!(usize, i64);
    let X = read![[i64]];
    let N = X.len();

    // 前の点との距離
    let mut D = vec![0; N];
    D[0] = X[0];
    for i in 1..N {
        D[i] = X[i] - X[i - 1];
    }

    let dp = vec![std::i64::MAX; N];
    let mut calc = Calc {
        e: e,
        X: X,
        D: D,
        dp: dp,
    };
    let cost = calc.calc(0);

    println!("{}", cost)
}
