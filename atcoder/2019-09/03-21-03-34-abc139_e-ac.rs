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

use Color::*;

/// 閉路検出中の頂点の色
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Color {
    /// 処理前の頂点の色
    White,

    /// 処理が実行中 (再帰処理の途中) の頂点の色
    Gray,

    /// 処理済みの頂点の色
    Black,
}

struct Solver {
    /// 選手数
    N: usize,

    /// A[u][i] = v : 選手 u が i 番目に選手 v と戦う。
    A: Vec<Vec<usize>>,

    /// order[u][v] = i : 選手 u が i 番目に選手 v と戦う。
    order: Vec<Vec<usize>>,

    /// opponents[u] : 選手 u の対戦相手が順番に出てくるキュー。
    opponents: Vec<VecDeque<usize>>,

    /// dirties : 前日の試合に参加した選手の集合。
    /// この集合に含まれる選手に限り、本日の試合に参加できる可能性がある。
    dirties: BTreeSet<usize>,
}

impl Solver {
    fn new(N: usize, A: Vec<Vec<usize>>) -> Solver {
        Solver {
            N: N,
            A: A,
            order: vec![vec![N; N]; N],
            opponents: vec![VecDeque::new(); N],
            dirties: BTreeSet::new(),
        }
    }

    /// 選手 u が選手 v と戦う試合の1つ前の試合で戦う相手
    fn prev_opponent(&self, u: usize, v: usize) -> Option<usize> {
        let ui = self.order[u][v];
        if ui == 0 {
            // u は v と最初に戦う
            return None;
        }

        Some(self.A[u][ui - 1])
    }

    /// u の次の対戦相手。試合できる状態でなければ None。
    fn ready_opponent(&self, u: usize) -> Option<usize> {
        let v = match self.opponents[u].get(0) {
            None => return None,
            Some(&v) => v,
        };

        match self.opponents[v].get(0) {
            None => unreachable!(),
            Some(&w) if w != u => {
                // v は u 以外の選手と先に戦うので、今日は u と試合できない。
                return None;
            }
            Some(_) => return Some(v),
        }
    }

    /// 試合日程を矛盾なく構築できるか？
    /// 試合の前後関係の循環 (閉路) がなければ OK。
    fn is_consistent(&self) -> bool {
        /// u, v の試合を探索する。閉路を見つけたら true を返す。
        fn dfs(mut u: usize, mut v: usize, solver: &Solver, colors: &mut Vec<Vec<Color>>) -> bool {
            if u > v {
                swap(&mut u, &mut v);
            }
            assert!(u < v);

            match colors[u][v] {
                Black => return false,
                Gray => {
                    // この試合の再帰処理中に再訪問しているので、閉路があったことになる。
                    return true;
                }
                White => {}
            }

            colors[u][v] = Gray;

            // u, v　が互いに相手と戦う直前の試合で戦う相手との試合を探索する。
            if let Some(w) = solver.prev_opponent(u, v) {
                if dfs(u, w, solver, colors) {
                    return true;
                }
            }

            if let Some(w) = solver.prev_opponent(v, u) {
                if dfs(v, w, solver, colors) {
                    return true;
                }
            }

            colors[u][v] = Black;

            false
        }

        let mut colors = vec![vec![White; self.N]; self.N];
        for u in 0..self.N {
            for v in u + 1..self.N {
                if dfs(u, v, self, &mut colors) {
                    // 閉路があったら矛盾
                    return false;
                }

                assert_eq!(colors[u][v], Black);
            }
        }

        // 無矛盾
        true
    }

    /// 最小の日数を求める。
    fn solve(mut self) -> Option<usize> {
        let N = self.N;

        // 準備
        for u in 0..N {
            for ui in 0..self.A[u].len() {
                let v = self.A[u][ui];
                self.order[u][v] = ui;
                self.opponents[u].push_back(v);
            }

            self.dirties.insert(u);
        }

        // 矛盾の検出
        if !self.is_consistent() {
            return None;
        }

        // 日程の構築
        let mut day_count = 0;
        let mut next_dirties = BTreeSet::new();

        loop {
            debug!(day_count);

            next_dirties.clear();

            for &u in self.dirties.iter() {
                let v = match self.ready_opponent(u) {
                    None => continue,
                    Some(v) => v,
                };

                debug!((u, v));

                next_dirties.insert(u);
                next_dirties.insert(v);
            }

            for &u in next_dirties.iter() {
                self.opponents[u].pop_front();
            }

            // 試合が行われなかったので終了。
            if next_dirties.is_empty() {
                break;
            }

            swap(&mut self.dirties, &mut next_dirties);
            day_count += 1;
        }

        assert!((0..N).all(|u| self.opponents[u].is_empty()));

        Some(day_count)
    }
}

fn main() {
    let N = read!(usize);
    let mut A = read![[usize]; N];

    // 選手の番号を 0-indexed に変換。
    for i in 0..N {
        for x in A[i].iter_mut() {
            *x -= 1;
        }
    }

    match Solver::new(N, A).solve() {
        None => println!("-1"),
        Some(day_count) => println!("{}", day_count),
    }
}
