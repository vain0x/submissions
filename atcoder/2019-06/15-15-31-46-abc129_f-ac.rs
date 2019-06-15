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

pub fn pow(x: i64, n: usize, M: i64) -> i64 {
    let (mut x, mut y, mut n) = (x % M, 1_i64, n);
    while n > 0 {
        if n % 2 != 0 {
            y = (y * x) % M;
            n -= 1;
        }

        x = (x * x) % M;
        n /= 2;
    }
    y
}

// 初項 A, 公差 B, 長さ L の等差数列
#[derive(Clone, Copy, Debug)]
struct Seq {
    A: i64,
    B: i64,
    L: usize,
}

// 3次元ベクトル
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct Vec3([i64; 3]);

// 3×3 行列
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct Mat3x3([[i64; 3]; 3]);

impl Seq {
    // i 番目の要素の値
    fn at(&self, i: usize) -> i64 {
        self.A + (i as i64) * self.B
    }

    /// n 以上の最小の要素の番号を二分探索する
    fn lower_bound(&self, n: i64) -> usize {
        if self.at(0) >= n {
            return 0;
        }

        let mut l = 0;
        let mut r = self.L; // ok
        while r - l > 1 {
            let m = l + (r - l) / 2;

            if self.at(m) >= n {
                r = m;
            } else {
                l = m;
            }
        }

        r
    }

    /// 桁数が d の要素の個数
    fn width(&self, d: usize) -> usize {
        let mut n = 1;
        for _ in 1..d {
            n *= 10;
        }

        let x = self.lower_bound(n);
        let y = self.lower_bound(n * 10);
        y - x
    }
}

impl Vec3 {
    fn at(&self, i: usize) -> i64 {
        self.0[i]
    }
}

impl Mat3x3 {
    fn unit() -> Self {
        Mat3x3([[1, 0, 0], [0, 1, 0], [0, 0, 1]])
    }

    fn at(&self, y: usize, x: usize) -> i64 {
        self.0[y][x]
    }

    fn mul_vec(&self, v: &Vec3, M: i64) -> Vec3 {
        let mut w = [0; 3];

        for y in 0..3 {
            let mut sum = 0;
            for t in 0..3 {
                sum += self.at(y, t) * v.at(t) % M;
                sum %= M;
            }
            w[y] = sum;
        }

        Vec3(w)
    }

    fn mul(&self, r: &Mat3x3, M: i64) -> Mat3x3 {
        let mut w = [[0; 3]; 3];

        for y in 0..3 {
            for x in 0..3 {
                let mut sum = 0;
                for t in 0..3 {
                    sum += self.at(y, t) * r.at(t, x) % M;
                    sum %= M;
                }
                w[y][x] = sum;
            }
        }

        Mat3x3(w)
    }

    pub fn pow(self, n: usize, M: i64) -> Self {
        let (mut x, mut y, mut n) = (self, Self::unit(), n);
        while n > 0 {
            if n % 2 != 0 {
                y = y.mul(&x, M);
                n -= 1;
            }

            x = x.mul(&x, M);
            n /= 2;
        }
        y
    }
}

fn main() {
    let (L, A, B, M) = read!(usize, i64, i64, i64);

    let seq = Seq { A: A, B: B, L: L };

    // 連結済みの要素の個数
    let mut l = 0;
    // 数列の要素を i 個連結して作った数
    let mut X = 0;
    // 数列の次の要素
    let mut s = A % M;

    for d in 1..19 {
        let n = min(L - l, seq.width(d));

        // 以下の遷移を n 回行う
        // X' = X * 10^d + s
        // s' =            s + B
        // 1  =                1

        let u = Vec3([X, s, 1]);
        let F = Mat3x3([[pow(10, d, M), 1, 0], [0, 1, B % M], [0, 0, 1]]);
        let v = F.pow(n, M).mul_vec(&u, M);

        X = v.at(0);
        s = v.at(1);
        l += n;
    }

    println!("{}", X)
}
