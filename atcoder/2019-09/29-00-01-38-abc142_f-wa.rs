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

/// 頂点
type Vertex = usize;

/// 頂点への探索経路
type Route = usize;

type Length = usize;

/// (p, q): p から q に合流するときにできる閉路
/// p: x -> ... -> v -> ... -> u (-> v)
/// q: x -> ... -> v
type Cycle = (Route, Route);

#[derive(Clone, Copy, Debug, PartialEq)]
enum State {
    /// 未訪問
    White,
    /// 訪問中
    Gray(Route),
    /// 訪問済み
    Black,
}

use State::*;

struct Solver {
    G: Vec<Vec<Vertex>>,
    links: Vec<(Vertex, Route, Length)>,
    states: Vec<State>,
    /// 発見した最短閉路
    mini: Option<(Length, Cycle)>,
}

impl Solver {
    fn route_len(&self, p: Route) -> Length {
        self.links[p].2
    }

    fn route_push(&mut self, v: Vertex, p: Route) -> Route {
        let q = self.links.len();
        let len = self.route_len(p) + 1;
        self.links.push((v, p, len));
        q
    }

    fn cycle_len(&self, (p, q): Cycle) -> Length {
        self.route_len(p) - self.route_len(q) + 1
    }

    fn cycle_to_vs(&self, (mut p, q): Cycle) -> Vec<Vertex> {
        let mut vs = vec![];
        while p != q {
            let (v, r, _) = self.links[p];
            vs.push(v);
            p = r;
        }
        vs
    }

    fn did_find_cycle(&mut self, c: Cycle) {
        let len = self.cycle_len(c);

        // 長さ2の閉路は条件を満たさない (入出次数が2以上になる) ので却下
        if len <= 2 {
            return;
        }

        if self.mini.map_or(false, |(min_len, _)| len >= min_len) {
            return;
        }

        self.mini = Some((len, c));
    }

    /// 閉路を探索する。
    fn dfs(&mut self, p: Route) {
        let (u, _, _) = self.links[p];
        match self.states[u] {
            Black => return,
            Gray(vp) => {
                self.did_find_cycle((p, vp));
                return;
            }
            White => {}
        }

        self.states[u] = Gray(p);

        for i in 0..self.G[u].len() {
            let v = self.G[u][i];
            if self.states[v] == Black {
                continue;
            }

            let q = self.route_push(v, p);
            self.dfs(q);
        }

        self.states[u] = Black;
    }

    fn solve(mut self) -> Option<Vec<Vertex>> {
        for u in 0..self.G.len() {
            if self.states[u] == Black {
                continue;
            }

            let p = self.route_push(u, 0);
            self.dfs(p);
        }

        self.mini.map(|(_, c)| self.cycle_to_vs(c))
    }
}

fn main() {
    let (N, M) = read!(usize, usize);
    let mut G = vec![vec![]; N];

    for _ in 0..M {
        let (u, v) = read!(usize, usize);
        G[u - 1].push(v - 1);
    }

    let vs_opt = Solver {
        G: G,
        states: vec![White; N],
        links: vec![(0, 0, 0); N],
        mini: None,
    }
    .solve();

    match vs_opt {
        None => println!("-1"),
        Some(vs) => {
            println!("{}", vs.len());
            for v in vs {
                println!("{}", v + 1)
            }
        }
    }
}
