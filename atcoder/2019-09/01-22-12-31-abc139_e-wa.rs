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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum State {
    White,
    Gray,
    Black,
}

use State::*;

struct Solver {
    A: Vec<Vec<usize>>,
    ord: Vec<Vec<usize>>,
    states: Vec<Vec<State>>,
    matches: Vec<(usize, usize)>,
    ok: bool,
}

impl Solver {
    fn new(A: Vec<Vec<usize>>) -> Solver {
        let N = A.len();

        let mut ord = vec![vec![0; N]; N];
        for u in 0..N {
            for ui in 0..N - 1 {
                let v = A[u][ui];
                ord[u][v] = ui;
            }
        }

        Solver {
            A: A,
            ord: ord,
            states: vec![vec![White; N]; N],
            matches: vec![],
            ok: true,
        }
    }

    // 選手 u が v と戦う順目
    fn ord(&self, u: usize, v: usize) -> usize {
        self.ord[u][v]
    }

    fn dfs(&mut self, u: usize, v: usize) {
        // 選手 u と v の試合を行いたい
        // 先に行わなければいけない試合をスケジュールする

        assert_ne!(u, v);

        if u > v {
            self.dfs(v, u);
            return;
        }

        if !self.ok {
            return;
        }

        let ui = self.ord(u, v);
        let vi = self.ord(v, u);

        match self.states[u][v] {
            White => {}
            Gray => {
                self.ok = false;
                return;
            }
            Black => return,
        }
        self.states[u][v] = Gray;

        // 先に済ませなければいけない試合をスケジュールする

        for uj in 0..ui {
            let w = self.A[u][uj];
            self.dfs(u, w);
        }

        for vj in 0..vi {
            let w = self.A[v][vj];
            self.dfs(v, w);
        }

        self.matches.push((u, v));
        debug!((u, v));

        self.states[u][v] = Black;
    }

    fn day_count(&self) -> usize {
        let N = self.A.len();

        let mut done_match = vec![vec![false; N]; N];
        let mut match_count = 0;
        let mut qs = vec![VecDeque::new(); N];

        for &(u, v) in &self.matches {
            qs[u].push_back((u, v));
            qs[v].push_back((u, v));
        }

        let mut done = vec![false; N];
        let mut day_count = 0;
        while match_count < self.matches.len() {
            debug!(day_count);
            for u in 0..N {
                done[u] = false;
            }

            for w in 0..N {
                while let Some(&(u, v)) = qs[w].get(0) {
                    debug!((u, v));
                    if done_match[u][v] {
                        qs[w].pop_front();
                        continue;
                    }

                    if !done[u] && !done[v] {
                        debug!((u, v));
                        qs[w].pop_front();
                        done_match[u][v] = true;
                        match_count += 1;
                        done[u] = true;
                        done[v] = true;
                        continue;
                    }

                    break;
                }
            }

            day_count += 1;
        }

        day_count
    }
}

fn main() {
    let N = read!(usize);
    let mut A = read![[usize]; N];

    let mut Q = BinaryHeap::new();

    for i in 0..N {
        for x in A[i].iter_mut() {
            *x -= 1;
        }

        Q.push((N - 1, i));
    }

    let mut solver = Solver::new(A);
    for u in 0..N {
        for v in u + 1..N {
            solver.dfs(u, v);
        }
    }

    if !solver.ok {
        println!("-1");
        return;
    }

    println!("{}", solver.day_count())
}
