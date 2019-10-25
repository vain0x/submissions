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

const INF: i64 = std::i64::MAX / 4;

fn main() {
    let (N, M, L) = read!(usize, usize, i64);

    let mut G = vec![vec![]; N];
    for (u, v, cost) in read![usize, usize, i64; M] {
        let (u, v) = (u - 1, v - 1);

        if cost > L {
            continue;
        }

        G[u].push((v, cost));
        G[v].push((u, cost));
    }

    // dist[u][v]: u から v へ補給なしで移動するときの燃料の総和の最小値
    let mut dist = vec![vec![INF; N]; N];

    for u in 0..N {
        dist[u][u] = 0;

        for &(v, cost) in &G[u] {
            dist[u][v] = cost;
        }
    }

    for w in 0..N {
        for u in 0..N {
            for v in 0..N {
                dist[u][v] = min(dist[u][v], dist[u][w] + dist[w][v]);
            }
        }
    }

    debug!(dist);

    // H: 満タンの状態で u から v へ補給なしで移動できるとき辺 u→v があるグラフ
    let mut H = vec![vec![]; N];
    for u in 0..N {
        for v in u + 1..N {
            if dist[u][v] <= L {
                H[u].push(v);
                H[v].push(u);
            }
        }
    }

    debug!(H);

    // count[u][v]: 満タンの状態で街 u から v へ移動するときの補給回数
    let mut count = vec![vec![INF; N]; N];
    for u in 0..N {
        count[u][u] = 0;

        for &v in &H[u] {
            count[u][v] = 1;
        }
    }

    for w in 0..N {
        for u in 0..N {
            for v in 0..N {
                count[u][v] = min(count[u][v], count[u][w] + count[w][v]);
            }
        }
    }

    debug!(count);

    let Q = read!(usize);
    for (s, t) in read![usize, usize; Q] {
        let (s, t) = (s - 1, t - 1);

        let n = count[s][t];
        if n < INF {
            println!("{}", n - min(n, 1));
        } else {
            println!("-1");
        }
    }
}
