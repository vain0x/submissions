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
    let T = read![usize, usize; M];
    let (s, t) = read!(usize, usize);

    let (s, t) = (s - 1, t - 1);

    let mut G = vec![vec![]; N];
    for (u, v) in T {
        G[u - 1].push(v - 1);
    }

    let mut done = vec![vec![false; 3]; N];
    let mut dist = vec![vec![None; 3]; N];
    let mut Q = VecDeque::new();
    Q.push_back((s, 0));

    while let Some((u, x)) = Q.pop_front() {
        let r = x % 3;

        if done[u][r] {
            continue;
        }

        done[u][r] = true;
        dist[u][r] = Some(x);

        debug!((u, r, x));

        for i in 0..G[u].len() {
            let v = G[u][i];
            let y = x + 1;
            Q.push_back((v, y));
        }
    }

    match dist[t][0] {
        None => println!("-1"),
        Some(d) => println!("{}", d / 3),
    }
}
