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

fn main() {
    let (N, M) = read!(usize, usize);
    let mut g = vec![vec![]; N];

    for ei in 0..M {
        let (a, b, c) = read!(usize, usize, i64);
        let (a, b) = (a - 1, b - 1);
        g[a].push((b, c, ei));
        g[b].push((a, c, ei));
    }

    let mut retain = vec![false; M];

    for u in 0..N {
        let mut q = BinaryHeap::new();
        const INF: i64 = 1 << 60;

        let mut dist = vec![INF; N];
        dist[u] = 0;
        q.push((u, 0));

        while let Some((u, d)) = q.pop() {
            for &(v, c, _) in &g[u] {
                let d = d + c;
                if dist[v] > d {
                    dist[v] = d;
                    q.push((v, d));
                }
            }
        }

        for v in 0..N {
            if v == u {
                continue;
            }

            for &(w, c, ei) in &g[v] {
                if dist[w] + c == dist[v] {
                    // 少なくとも1つの最短経路に含まれているので削除しない。
                    retain[ei] = true;
                }
            }
        }
    }

    let count = M - (0..M).filter(|&i| retain[i]).count();

    println!("{}", count)
}
