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

#[derive(Clone, Copy)]
enum State {
    White,
    Gray,
    Black,
}

use State::*;

struct Graph {
    G: Vec<Vec<usize>>,
    prev: Vec<usize>,
    state: Vec<State>,
}

impl Graph {
    fn dfs(&mut self, u: usize) -> bool {
        match self.state[u] {
            Black => return false,
            Gray => return true,
            White => {}
        }

        self.state[u] = Gray;

        for i in 0..self.G[u].len() {
            let v = self.G[u][i];

            if self.dfs(v) {
                self.prev[v] = u;
                return true;
            }
        }

        self.state[u] = Black;

        false
    }

    fn solve(mut self) -> Option<Vec<usize>> {
        for u in 0..self.G.len() {
            if self.dfs(u) {
                let mut w = vec![];
                let mut v = u;
                loop {
                    w.push(v);
                    v = self.prev[v];
                    if v == u {
                        break;
                    }
                }
                return Some(w);
            }
        }

        None
    }
}

fn main() {
    let (N, M) = read!(usize, usize);
    let mut G = vec![vec![]; N];
    for _ in 0..M {
        let (u, v) = read!(usize, usize);
        G[u - 1].push(v - 1);
    }

    let w = Graph {
        G: G,
        state: vec![White; N],
        prev: vec![N; N],
    }
    .solve();

    match w {
        None => println!("-1"),
        Some(w) => {
            println!("{}", w.len());
            for v in w {
                println!("{}", v + 1)
            }
        }
    }
}
