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

/// s & (1 << i) != 0 <=> エッジ el + i が黒に塗られている (i < er - el)
/// 満たされる制約の集合を返す。
fn bfs(
    s: usize,
    el: usize,
    er: usize,
    G: &Vec<Vec<usize>>,
    edge_map: &Vec<Vec<usize>>,
    C: &Vec<(usize, usize)>,
) -> usize {
    let mut x = 0;
    let N = G.len();

    let mut q = VecDeque::new();
    for ci in 0..C.len() {
        let (u, _) = C[ci];
        q.push_back((u, ci, N, false));
    }

    while let Some((u, ci, p, b)) = q.pop_front() {
        if u == C[ci].1 && b {
            x |= 1 << ci;
            continue;
        }

        for ei in 0..G[u].len() {
            let v = G[u][ei];
            if v == p {
                continue;
            }

            // 黒のエッジを通る？
            let b = b || {
                let e = edge_map[u][v];
                el <= e && e < er && (s & (1 << (e - el))) != 0
            };

            q.push_back((v, ci, u, b));
        }
    }

    x
}

fn main() {
    let N = read!(usize);

    let mut G = vec![vec![]; N];
    for (mut u, mut v) in read![usize, usize; N - 1] {
        u -= 1;
        v -= 1;
        G[u].push(v);
        G[v].push(u);
    }

    let M = read!(usize);
    let mut C = read![usize, usize; M];
    for &mut (ref mut u, ref mut v) in &mut C {
        *u -= 1;
        *v -= 1;
    }

    let mut edges = vec![];
    let mut edge_map = vec![vec![N; N]; N];

    for u in 0..N {
        for ei in 0..G[u].len() {
            let v = G[u][ei];
            if u < v {
                edge_map[u][v] = edges.len();
                edge_map[v][u] = edges.len();
                edges.push((u, v));
            }
        }
    }
    debug!(edges, edge_map);

    let em = (N - 1) / 2;
    let el = 0;
    let er = em;
    let mut dp = vec![0_usize; 1 << M];

    for s in 0..(1 << (er - el)) {
        let x = bfs(s, el, er, &G, &edge_map, &C);
        debug!((s, x));
        dp[x] += 1;
    }

    debug!(dp);

    // 累積和
    for pc in (0..M + 1).rev() {
        for x in 0..1 << M {
            if (0..M).filter(|&i| (x & (1 << i)) != 0).count() == pc {
                for i in 0..M {
                    if x & (1 << i) != 0 {
                        dp[x & !(1 << i)] += dp[x];
                    }
                }
            }
        }
    }

    debug!(dp);

    let el = em;
    let er = N - 1;
    let mut count = 0;

    for s in 0..(1 << (er - el)) {
        let y = bfs(s, el, er, &G, &edge_map, &C);
        debug!((s, y));

        let x = ((1 << M) - 1) & !y;
        count += dp[x];
    }

    println!("{}", count)
}
