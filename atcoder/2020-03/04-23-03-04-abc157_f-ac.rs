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

const EPS: f64 = 1e-8;

fn sq(x: f64) -> f64 {
    x * x
}

#[derive(Clone, Copy)]
struct Point {
    x: f64,
    y: f64,
}

impl Point {
    fn distance(self, other: Point) -> f64 {
        (sq(self.x - other.x) + sq(self.y - other.y)).sqrt()
    }
}

#[derive(Clone, Copy)]
struct Circle {
    p: Point,
    r: f64,
}

impl Circle {
    fn crosspoints(self, other: Circle) -> [Point; 2] {
        let (c1, c2) = (self, other);

        let d = c1.p.distance(c2.p);
        let a = ((sq(d) + sq(c1.r) - sq(c2.r)) / (2.0 * c1.r * d)).acos();
        let t = (c2.p.y - c1.p.y).atan2(c2.p.x - c1.p.x);

        let p = |a: f64| Point {
            x: c1.p.x + c1.r * (t + a).cos(),
            y: c1.p.y + c1.r * (t + a).sin(),
        };

        [p(a), p(-a)]
    }
}

#[derive(Clone, Copy)]
struct Meat {
    x: f64,
    y: f64,
    c: f64,
}

impl Meat {
    fn from((x, y, c): (f64, f64, f64)) -> Self {
        Meat { x: x, y: y, c: c }
    }

    fn point(&self) -> Point {
        Point {
            x: self.x,
            y: self.y,
        }
    }

    /// T 秒以内に肉が焼けるような熱源の位置からなる円盤
    fn extent(self, T: f64) -> Circle {
        Circle {
            p: self.point(),
            r: T / self.c,
        }
    }

    /// 位置 h に熱源があるとき、T 秒後にこんがり焼けるか？
    fn well_done(self, h: Point, T: f64) -> bool {
        self.c * self.point().distance(h) < T + EPS
    }
}

fn solve(meats: &[Meat], K: usize) -> f64 {
    if K <= 1 {
        // 肉の真下に熱源を置くと一瞬で黒焦げ
        return 0.0;
    }

    let N = meats.len();

    let mut l = 0.0;
    let mut r = 1e9;

    // 最適な熱源の位置の候補の集合
    let mut heat_sources = vec![];

    for _ in 0..100 {
        let T = l + (r - l) / 2.0;
        debug!((l, r, T));

        debug_assert!(heat_sources.is_empty());

        for i in 0..N {
            heat_sources.push(meats[i].point());

            for j in i + 1..N {
                let e1 = meats[i].extent(T);
                let e2 = meats[j].extent(T);

                for &p in &e1.crosspoints(e2) {
                    heat_sources.push(p);
                }
            }
        }

        let ok = heat_sources.drain(..).any(|heat_source| {
            meats
                .iter()
                .filter(|meat| meat.well_done(heat_source, T))
                .take(K)
                .count()
                >= K
        });

        if ok {
            r = T;
        } else {
            l = T;
        }
    }

    r
}

fn main() {
    let (N, K) = read!(usize, usize);
    let meats = read![f64, f64, f64; N]
        .into_iter()
        .map(Meat::from)
        .collect::<Vec<_>>();

    println!("{}", solve(&meats, K))
}
