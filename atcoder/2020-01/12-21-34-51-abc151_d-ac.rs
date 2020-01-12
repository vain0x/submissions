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

type V = (usize, usize);
type Graph = Vec<Vec<Vec<V>>>;

fn bfs(u: V, dist: &mut Vec<Vec<Option<usize>>>, g: &Graph) {
    let mut Q = VecDeque::new();
    Q.push_back((u, 0));

    while let Some((u, d)) = Q.pop_front() {
        if dist[u.0][u.1].is_some() {
            continue;
        }

        dist[u.0][u.1] = Some(d);

        for i in 0..g[u.0][u.1].len() {
            let v = g[u.0][u.1][i];
            if dist[v.0][v.1].is_some() {
                continue;
            }

            Q.push_back((v, d + 1));
        }
    }
}

fn most_distant(dist: &Vec<Vec<Option<usize>>>, H: usize, W: usize) -> usize {
    let mut d_max = 0;
    let mut v = (0, 0);

    for y in 0..H {
        for x in 0..W {
            if let Some(d) = dist[y][x] {
                if d_max <= d {
                    d_max = d;
                    v = (y, x);
                }
            }
        }
    }

    d_max
}

fn main() {
    let (H, W) = read!(usize, usize);
    let board = read![String; H]
        .into_iter()
        .map(|s| s.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();

    let mut G = vec![vec![vec![]; W]; H];

    for y in 0..H {
        for x in 0..W {
            if board[y][x] == '.' {
                if y + 1 < H && board[y + 1][x] == '.' {
                    G[y][x].push((y + 1, x));
                    G[y + 1][x].push((y, x));
                }
                if x + 1 < W && board[y][x + 1] == '.' {
                    G[y][x].push((y, x + 1));
                    G[y][x + 1].push((y, x));
                }
            }
        }
    }

    let mut dist = vec![vec![None; W]; H];
    let mut d_max = 0;
    for y in 0..H {
        for x in 0..W {
            for y in 0..H {
                for x in 0..W {
                    dist[y][x] = None;
                }
            }

            bfs((y, x), &mut dist, &G);
            let d = most_distant(&dist, H, W);
            d_max = max(d_max, d);
        }
    }

    println!("{}", d_max)
}
