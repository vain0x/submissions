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
    let (N, M, P) = read!(usize, usize, i64);

    let mut g = vec![vec![]; N];
    for _ in 0..M {
        let (A, B, C) = read!(usize, usize, i64);
        let (A, B) = (A - 1, B - 1);
        g[A].push((B, C - P));
    }

    // 0 から到達できない頂点を削る。
    let mut reachable = vec![false; N];
    let mut stack = vec![0];
    reachable[0] = true;

    while let Some(u) = stack.pop() {
        for i in 0..g[u].len() {
            let (v, _) = g[u][i];
            if reachable[v] {
                continue;
            }

            reachable[v] = true;
            stack.push(v);
        }
    }

    for u in 0..N {
        g[u].retain(|&(v, _)| reachable[u] && reachable[v]);
    }

    // N に到達できない頂点を削る。(N から辺を逆順に辿って到達できない頂点を削る。)
    let mut valid = vec![false; N];

    let mut h = vec![vec![]; N];
    for u in 0..N {
        for i in 0..g[u].len() {
            let (v, _) = g[u][i];
            h[v].push(u);
        }
    }

    stack.clear();
    stack.push(N - 1);
    valid[N - 1] = true;

    while let Some(u) = stack.pop() {
        for i in 0..h[u].len() {
            let v = h[u][i];
            if valid[v] {
                continue;
            }

            valid[v] = true;
            stack.push(v);
        }
    }

    for u in 0..N {
        g[u].retain(|&(v, _)| valid[u] && valid[v]);
    }

    // Bellman-Ford
    const INF: i64 = 1 << 60;
    let mut dist = vec![-INF; N];
    let mut ok = true;

    dist[0] = 0;

    for k in 0..N {
        for u in 0..N {
            for i in 0..g[u].len() {
                let (v, c) = g[u][i];

                if dist[v] < dist[u] + c {
                    dist[v] = dist[u] + c;

                    if k == N - 1 {
                        // 負閉路がある。
                        ok = false;
                    }
                }
            }
        }
    }

    if ok {
        println!("{}", max(dist[N - 1], 0));
    } else {
        println!("-1");
    }
}
