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

// 問題概要: 品物が N 個ある。i 番目の品物の種類は t で、価値は d である。品物を K 個選んだ。選ばれた品物の種類数を x とし、価値の総和を s とするとき、スコア (s + x^2) の値が (選びかたの中で) 最大だった。この最大スコアを求めよ。

fn main() {
    let (N, K) = read!(usize, usize);
    let T = read![usize, i64; N];

    let mut T = T.into_iter().map(|(t, d)| (d, t - 1)).collect::<Vec<_>>();
    T.sort();
    T.reverse();

    debug!(T);

    let mut s = 0_i64;
    let mut x = 0_i64;
    let mut Q = vec![];

    // done[t] = true : 種類 t の品物を選んでいる
    let mut done = vec![false; N + 1];

    for i in 0..K {
        let (d, t) = T[i];

        if done[t] {
            // 同じ種類の2個目以降の品物は選ばないほうがいい可能性がある
            Q.push(d);
        } else {
            // この種類で価値が最大の品物は必ず選ぶべきなので Q には入れない
            done[t] = true;
            x += 1;
        }

        s += d;
    }

    let mut max_score = s + x * x;
    debug!(s, x, max_score);

    for i in K..N {
        let (d, t) = T[i];

        if done[t] {
            continue;
        }

        // この品物はまだ選んでいない。

        // 同じ種類の品物で2つ以上選んでいるもののうち、価値が最も低いものを選ばないことにする。
        if let Some(d) = Q.pop() {
            s -= d;
        } else {
            // 除去できる品物がないなら終了。
            break;
        }

        // この品物を選ぶ。価値の合計は減るが、種類数が増えるので、スコアが大きくなる可能性がある。
        x += 1;
        s += d;
        done[t] = true;
        let score = s + x * x;

        max_score = max(max_score, score);
        debug!(d, x, s, x * x, score, max_score);
    }

    println!("{}", max_score);
}
