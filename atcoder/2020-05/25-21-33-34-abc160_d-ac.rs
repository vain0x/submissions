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
    let (N, X, Y) = read!(usize, usize, usize);
    let (X, Y) = (X - 1, Y - 1);

    let mut G = vec![vec![]; N];
    for i in 0..N - 1 {
        G[i].push(i + 1);
        G[i + 1].push(i);
    }

    G[X].push(Y);
    G[Y].push(X);

    let mut q = VecDeque::new();
    let mut dist = vec![];
    let mut ans = vec![0; N];

    for s in 0..N {
        dist.clear();
        dist.resize(N, N);

        // BFS
        q.push_back((s, 0));
        dist[s] = 0;

        while let Some((u, d)) = q.pop_front() {
            let d = d + 1;

            for i in 0..G[u].len() {
                let v = G[u][i];

                if dist[v] > d {
                    dist[v] = d;
                    q.push_back((v, d));
                }
            }
        }

        for t in s + 1..N {
            // s-t 間の最短経路が k=dist[t] であるペアを1つ見つけた。
            ans[dist[t]] += 1;
        }
    }

    for k in 1..N {
        println!("{}", ans[k]);
    }
}
