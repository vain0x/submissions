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

pub fn bellman_ford(s: usize, g: &[Vec<(usize, i64)>], dist: &mut [i64], neg: &mut bool) {
    let n = g.len();

    for i in 0..n {
        dist[i] = if i == s { 0 } else { std::i64::MAX };
    }
    *neg = false;

    for k in 0..n {
        for u in 0..n {
            for i in 0..g[u].len() {
                let (v, w) = g[u][i];
                if dist[v] - w <= dist[u] {
                    continue;
                }

                dist[v] = dist[u] + w;
                if k == n - 1 {
                    *neg = true;
                }
            }
        }
    }
}

fn reachable<W>(s: usize, g: &[Vec<(usize, W)>]) -> Vec<bool> {
    let n = g.len();

    let mut done = vec![false; n];
    let mut q = VecDeque::new();
    q.push_back(s);

    while let Some(u) = q.pop_front() {
        if done[u] {
            continue;
        }

        done[u] = true;

        for i in 0..g[u].len() {
            let &(v, _) = &g[u][i];
            q.push_back(v);
        }
    }

    done
}

pub fn prune<W>(s: usize, t: usize, g: &mut [Vec<(usize, W)>]) {
    let n = g.len();

    let done = reachable(s, g);
    for es in g.iter_mut() {
        es.retain(|&(v, _)| done[v]);
    }

    let mut dual = vec![vec![]; n];
    for u in 0..n {
        for i in 0..g[u].len() {
            let (v, ref w) = g[u][i];
            dual[v].push((u, w.clone()));
        }
    }

    let done = reachable(t, &dual);
    for es in g.iter_mut() {
        es.retain(|&(v, _)| done[v]);
    }
}

fn main() {
    let (N, M, P) = read!(usize, usize, i64);
    let T = read![usize, usize, i64; M];

    let mut G = vec![vec![]; N];
    for (u, v, w) in T {
        G[u - 1].push((v - 1, P - w));
    }

    prune(0, N - 1, &mut G);

    let mut dist = vec![0; N];
    let mut neg = false;
    bellman_ford(0, &G, &mut dist, &mut neg);

    if neg {
        println!("-1")
    } else {
        println!("{}", max(0, -dist[N - 1]))
    }
}
