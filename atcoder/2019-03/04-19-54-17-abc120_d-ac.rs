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
    buf.trim_right().to_owned()
}

// -----------------------------------------------
// Solution
// -----------------------------------------------

pub struct UnionFind {
    nodes: Vec<UnionFindNode>,
}

enum UnionFindNode {
    Root { size: usize },
    Child { parent: RefCell<usize> },
}

impl UnionFind {
    pub fn new(size: usize) -> Self {
        UnionFind {
            nodes: (0..size)
                .map(|_| UnionFindNode::Root { size: 1 })
                .collect::<Vec<_>>(),
        }
    }

    pub fn root_node(&self, v: usize) -> (usize, usize) {
        match &self.nodes[v] {
            &UnionFindNode::Root { size } => (v, size),
            &UnionFindNode::Child { parent: ref p } => {
                let (u, size) = self.root_node(*p.borrow());
                *p.borrow_mut() = u;
                (u, size)
            }
        }
    }

    pub fn root(&self, v: usize) -> usize {
        self.root_node(v).0
    }

    pub fn size(&self, v: usize) -> usize {
        self.root_node(v).1
    }

    pub fn connects(&self, u: usize, v: usize) -> bool {
        self.root(u) == self.root(v)
    }

    pub fn merge(&mut self, u: usize, v: usize) {
        let (u, u_size) = self.root_node(u);
        let (v, v_size) = self.root_node(v);

        if u == v {
            return;
        }

        if u_size < v_size {
            self.merge(v, u);
            return;
        }

        self.nodes[v] = UnionFindNode::Child {
            parent: RefCell::new(u),
        };
        self.nodes[u] = UnionFindNode::Root {
            size: u_size + v_size,
        };
    }
}

fn main() {
    let (N, M) = read!(usize, usize);
    let T = read!(usize, usize; M);

    // C[m] = (m 個の辺を削除したときの不便さ)
    let mut C = vec![0; M];

    // ループの変数 m に関する状態を記録する変数
    let mut c = N * (N - 1) / 2;
    let mut uf = UnionFind::new(N);

    for m in (0..M).rev() {
        C[m] = c;

        let (u, v) = T[m];
        // 1-indexed から 0-indexed への変換
        let (u, v) = (u - 1, v - 1);

        // 同一連結成分内に辺を追加しても不便さは減らない
        if uf.connects(u, v) {
            continue;
        }

        // 異なる連結成分の間に辺を追加すると不便さが減る
        c -= uf.size(u) * uf.size(v);
        uf.merge(u, v);
    }

    for m in 0..M {
        println!("{}", C[m]);
    }
}
