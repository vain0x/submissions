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

fn reach(t: usize, g: &mut [Vec<(usize, i64)>], reachable: &mut [bool]) {
    let n = g.len();

    let mut r = vec![vec![]; n];
    for u in 0..n {
        for i in 0..g[u].len() {
            let (v, _) = g[u][i];
            r[v].push(u);
        }
    }

    let mut q = VecDeque::new();
    q.push_back(t);

    while let Some(u) = q.pop_front() {
        if reachable[u] {
            continue;
        }

        reachable[u] = true;

        for i in 0..r[u].len() {
            let v = r[u][i];
            q.push_back(v);
        }
    }
}

fn bellman_ford(s: usize, g: &[Vec<(usize, i64)>], dist: &mut [i64], infinite: &mut bool) {
    let n = g.len();

    for i in 0..n {
        dist[i] = if i == s { 0 } else { std::i64::MAX };
    }
    *infinite = false;

    for k in 0..n {
        for u in 0..n {
            for i in 0..g[u].len() {
                let (v, w) = g[u][i];
                if dist[u] == std::i64::MAX || dist[v] <= dist[u] + w {
                    continue;
                }

                dist[v] = dist[u] + w;
                if k == n - 1 {
                    dist[v] = std::i64::MIN;
                    *infinite = true;
                }
            }
        }
    }
}

fn main() {
    let (N, M, P) = read!(usize, usize, i64);
    let T = read![usize, usize, i64; M];

    let mut G = vec![vec![]; N];
    for (u, v, w) in T {
        G[u - 1].push((v - 1, P - w));
    }

    let mut reachable = vec![false; N];
    reach(N - 1, &mut G, &mut reachable);

    for u in 0..N {
        G[u].retain(|&(v, _)| reachable[v]);
    }

    let mut dist = vec![0; N];
    let mut infinite = false;
    bellman_ford(0, &G, &mut dist, &mut infinite);

    if infinite {
        println!("-1")
    } else {
        println!("{}", max(0, -dist[N - 1]))
    }
}
