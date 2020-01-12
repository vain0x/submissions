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

type P = (f64, f64);

fn sq(x: f64) -> f64 {
    x * x
}

fn dist(p: P, q: P) -> f64 {
    let (x1, y1) = p;
    let (x2, y2) = q;
    (sq(x1 - x2) + sq(y1 - y2)).sqrt()
}

fn in_circle(p: P, c: P, r: f64) -> bool {
    let (px, py) = p;
    let (cx, cy) = c;

    sq(px - cx) + sq(py - cy) - sq(r) <= 0.0
}

fn main() {
    let N = read!(usize);
    let T = read![i64, i64; N]
        .into_iter()
        .map(|(x, y)| (x as f64, y as f64))
        .collect::<Vec<_>>();

    let mut min_r = std::f64::MAX;

    for s in 0..N {
        for t in s + 1..N {
            let (x1, y1) = T[s];
            let (x2, y2) = T[t];

            let cx = (x1 + x2) / 2.0;
            let cy = (y1 + y2) / 2.0;

            let c = (cx, cy);
            let r = dist(T[s], c);
            debug!((T[s], T[t]), (c, r));

            if min_r <= r {
                continue;
            }

            let ok = (0..N).all(|i| in_circle(T[i], c, r));
            if ok {
                min_r = r;
            }
        }
    }

    if N >= 3 {
        for s in 0..N {
            for t in s + 1..N {
                for u in t + 1..N {
                    let (x1, y1) = T[s];
                    let (x2, y2) = T[t];
                    let (x3, y3) = T[u];

                    let cx = ((y1 - y3) * (sq(y1) - sq(y2) + sq(x1) - sq(x2))
                        - (y1 - y2) * (sq(y1) - sq(y3) + sq(x1) - sq(x3)))
                        / (2.0 * (y1 - y3) * (x1 - x2) - 2.0 * (y1 - y2) * (x1 - x3));

                    let cy = ((x1 - x3) * (sq(x1) - sq(x2) + sq(y1) - sq(y2))
                        - (x1 - x2) * (sq(x1) - sq(x3) + sq(y1) - sq(y3)))
                        / (2.0 * (x1 - x3) * (y1 - y2) - 2.0 * (x1 - x2) * (y1 - y3));

                    let c = (cx, cy);
                    let r = dist(T[s], c);
                    debug!((T[s], T[t], T[u]), (c, r));

                    if min_r <= r {
                        continue;
                    }

                    let ok = (0..N).all(|i| in_circle(T[i], c, r));
                    if ok {
                        min_r = r;
                    }
                }
            }
        }
    }

    println!("{}", min_r)
}
