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

fn sq(x: f64) -> f64 {
    x * x
}

fn main() {
    let (N, K) = read!(usize, usize);
    let meets = read![f64, f64, f64; N];

    let mut l = 0.0;
    let mut r = 1e9;
    let mut points = vec![];

    for _ in 0..100 {
        let T = (l + r) / 2.0;
        debug!((l, r, T));

        points.clear();
        for i in 0..N {
            for j in i + 1..N {
                for &s in &[1.0, -1.0] {
                    let (x1, y1, c1) = meets[i];
                    let (x2, y2, c2) = meets[j];

                    points.push((
                        (c2 * x1 + c1 * x2) / (c1 + c2),
                        (c2 * y1 + c1 * y2) / (c1 + c2),
                    ));

                    // 平行移動
                    let (x1, y1) = (x1 - x2, y1 - y2);

                    // (x1, y1) と (0, 0) を中心とする半径 r2, r1 の円の交点を求める。
                    let r2 = T / c1;
                    let r1 = T / c2;
                    let v = sq(x1) + sq(y1);
                    let a = (v + sq(r1) - sq(r2)) / 2.0;
                    let u = (v * sq(r1) - sq(a)).sqrt();
                    let X = (a * x1 + s * y1 * u) / v;
                    let Y = (a * y1 - s * x1 * u) / v;

                    let (X, Y) = (X + x2, Y + y2);

                    points.push((X, Y));
                }
            }
        }

        let mut ok = false;
        'a: for (X, Y) in points.drain(..) {
            let mut count = 0;
            for i in 0..N {
                let (x, y, c) = meets[i];
                if sq(c) * (sq(x - X) + sq(y - Y)) <= sq(T) + 1e-7 {
                    count += 1;
                    if count >= K {
                        debug!((T, X, Y));
                        ok = true;
                        break 'a;
                    }
                }
            }
        }

        if ok {
            r = T;
        } else {
            l = T;
        }
    }

    println!("{}", l)
}
