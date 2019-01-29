// -----------------------------------------------
// Framework <https://github.com/vain0x/procon>
//
// See the bottom of file for solution.
// -----------------------------------------------

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

#[allow(dead_code)]
fn rl() -> String {
    let mut buf = String::new();
    stdin().read_line(&mut buf).unwrap();
    buf.trim_right().to_owned()
}

// -----------------------------------------------
// Solution
// -----------------------------------------------

pub struct UnionFindForest {
    nodes: Vec<UffNode>,
}

enum UffNode {
    Root(usize),
    Child(usize),
}

impl UnionFindForest {
    pub fn new(size: usize) -> Self {
        UnionFindForest {
            nodes: (0..size).map(|_| UffNode::Root(1)).collect::<Vec<_>>(),
        }
    }

    pub fn root_node(&mut self, v: usize) -> (usize, usize) {
        match self.nodes[v] {
            UffNode::Root(rank) => (v, rank),
            UffNode::Child(u) => {
                let (u, rank) = self.root_node(u);
                self.nodes[v] = UffNode::Child(u);
                (u, rank)
            }
        }
    }

    pub fn root(&mut self, v: usize) -> usize {
        self.root_node(v).0
    }

    pub fn connects(&mut self, u: usize, v: usize) -> bool {
        self.root(u) == self.root(v)
    }

    pub fn merge(&mut self, u: usize, v: usize) {
        let (u, u_rank) = self.root_node(u);
        let (v, v_rank) = self.root_node(v);

        if u == v {
            return;
        }

        if u_rank < v_rank {
            self.merge(v, u);
            return;
        }

        self.nodes[v] = UffNode::Child(u);
        self.nodes[u] = UffNode::Root(u_rank + 1);
    }
}

fn main() {
    let (N, M) = read!(usize, usize);
    let X = read![[i64]];
    let T = read![usize, usize, i64; M];

    let mut E = T
        .into_iter()
        .enumerate()
        .map(|(e, (u, v, w))| (w, u - 1, v - 1, e))
        .collect::<Vec<_>>();
    E.sort();

    let mut G = vec![vec![]; N];
    for &(w, u, v, e) in &E {
        G[u].push((w, u, v, e));
        G[v].push((w, v, u, e));
    }

    let mut uff = UnionFindForest::new(N);
    let mut weights = X.clone();
    let mut maybe_max = vec![false; M];

    // N 頂点グラフに、コスト昇順に辺を追加していく
    for &(w, u, v, e) in &E {
        // この辺を含む、コスト w 以下の辺からなる連結成分の重み
        let mut sum = weights[uff.root(u)];

        // 辺を追加して、追加後の連結成分の重みを計算する
        if uff.root(u) != uff.root(v) {
            sum += weights[uff.root(v)];
            uff.merge(u, v);
            weights[uff.root(u)] = sum;

            debug!((e, u, v, w, sum));
        }

        // コスト w 以下の辺からなる連結成分がこの辺を支えられるなら、この辺は最終的な連結成分の重み最大辺である可能性がある
        if w <= sum {
            maybe_max[e] = true;
        }
    }

    debug!(maybe_max);

    E.reverse();

    // done[e] = n : 辺 e を残すかどうか
    // n=0: 未定
    // n=1: 残すことに決定
    // n=2: 捨てることに決定
    let mut done = vec![0; M];

    // コスト降順に辺をみていく
    for &(w, u, v, e) in &E {
        if done[e] != 0 {
            continue;
        }

        if !maybe_max[e] {
            done[e] = 2;
            continue;
        }

        // 残すことに決定
        done[e] = 1;
        // コスト w 以下で到達できるすべての辺も残す
        let mut stack = vec![u, v];
        while let Some(u) = stack.pop() {
            debug!((w, u));

            for i in 0..G[u].len() {
                let (w2, _, v, e) = G[u][i];
                if done[e] != 0 || w2 > w {
                    continue;
                }
                done[e] = 1;
                stack.push(v);
            }
        }
    }

    let score = done.iter().filter(|&&x| x != 1).count();
    println!("{}", score)
}
