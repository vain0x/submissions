//! ----------------------------------------------
//! Framework <https://github.com/vain0x/procon>
//!
//! See the bottom of file for solution.
//! ----------------------------------------------

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

/// Print values to standard error if debug mode.
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

/// Read from standard input and parse each word.
/// - `read!(T, U, ..)` parses a line as a tuple of words.
/// - `read![[T]]` parses a line as an array of words.
/// - `read![..; N]` parses `N` lines, using `read!(..)` repeatedly.
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

/// Read a line from standard input.
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

type Point = (i64, i64);

fn square(x: i64) -> i64 {
    x * x
}

fn distance(p: Point, q: Point) -> f64 {
    let (px, py) = p;
    let (qx, qy) = q;

    ((square(px - qx) + square(py - qy)) as f64).sqrt()
}

struct Solver {
    /// 街の個数
    N: usize,
    /// 街の座標のリスト
    points: Vec<Point>,

    //.AsMut perm に含まれる街の数
    k: usize,

    /// perm[0..k] は街の順列の一部。
    /// 最初に perm[0] に訪れて、
    /// そこから perm[1], perm[2], ... の順番で移動する、
    /// ということが決定事項であることを表す。
    perm: Vec<usize>,

    /// 街 x がすでに perm に含まれていたら done[x] = true
    done: Vec<bool>,

    /// 移動距離の総和
    total: f64,
    /// 経路数 (最終的に N! になる)
    count: i64,
}

impl Solver {
    fn distance(&self) -> f64 {
        let mut sum = 0.0;
        for w in self.perm.windows(2) {
            sum += distance(self.points[w[0]], self.points[w[1]]);
        }
        sum
    }

    // perm[0..i] が街の順列の一部になっていて、
    // 残りの perm[i..N]
    fn dfs(&mut self) {
        let N = self.N;

        // 移動経路が1つに確定
        if self.k == N {
            self.total += self.distance();
            self.count += 1;
            return;
        }

        // 次に移動する街を全通り試す。
        for i in 0..N {
            // 移動済みの街は訪れない
            if self.done[i] {
                continue;
            }

            self.perm[self.k] = i;
            self.done[i] = true;
            self.k += 1;

            // 残りの移動順を決めて移動経路の総和を増やす。
            self.dfs();

            // 状態を戻す。
            self.k -= 1;
            self.done[i] = false;
        }
    }

    fn solve(&mut self) -> f64 {
        self.dfs();

        // 平均
        self.total / self.count as f64
    }
}

fn main() {
    let N = read!(usize);
    let points = read![i64, i64; N];

    let average = Solver {
        N: N,
        points: points,
        k: 0,
        perm: vec![0; N],
        done: vec![false; N],
        total: 0.0,
        count: 0,
    }
    .solve();

    println!("{}", average)
}
