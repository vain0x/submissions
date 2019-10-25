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

#[derive(Clone, Copy, Debug, PartialEq)]
enum State {
    White,
    Gray(usize),
    Black,
}

use State::*;

struct Graph {
    G: Vec<Vec<usize>>,
    link: Vec<(usize, usize, usize)>,
    state: Vec<State>,
}

impl Graph {
    fn len(&mut self, xs: usize) -> usize {
        self.link[xs].2
    }

    fn cons(&mut self, head: usize, tail: usize) -> usize {
        let i = self.link.len();
        let len = self.len(tail) + 1;
        self.link.push((head, tail, len));
        i
    }

    fn to_vec(&mut self, mut xs: usize, ys: usize) -> Vec<usize> {
        let mut vec = vec![];
        while xs != ys {
            let (head, tail, _) = self.link[xs];
            vec.push(head);
            xs = tail;
        }
        vec
    }

    // 経路 up が部分経路 vp に接続するときの閉路の長さ
    fn cycle_len(&mut self, up: usize, vp: usize) -> usize {
        let (_, _, un) = self.link[up];
        let (_, _, vn) = self.link[vp];
        un - vn + 1
    }

    fn dfs(&mut self, up: usize) -> (usize, usize, usize) {
        let mut mini = (std::usize::MAX, 0, 0);

        let (u, _, _) = self.link[up];
        match self.state[u] {
            Black => return mini,
            Gray(vp) => {
                let new_cn = self.cycle_len(up, vp);
                return (new_cn, up, vp);
            }
            White => {}
        }

        self.state[u] = Gray(up);

        for i in 0..self.G[u].len() {
            let v = self.G[u][i];
            if self.state[v] == Black {
                continue;
            }

            let vp = self.cons(v, up);
            let other = self.dfs(vp);
            mini = min(mini, other);
        }

        self.state[u] = Black;
        mini
    }

    fn solve(mut self) -> Option<Vec<usize>> {
        let mut mini = (std::usize::MAX, 0, 0);

        for u in 0..self.G.len() {
            if self.state[u] == Black {
                continue;
            }

            let other = self.dfs(u);
            mini = min(mini, other);
        }

        let (min_len, up, vp) = mini;
        if min_len == std::usize::MAX {
            return None;
        }

        debug!(up, vp);
        Some(self.to_vec(up, vp))
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
        link: vec![(0, 0, 0); N],
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
