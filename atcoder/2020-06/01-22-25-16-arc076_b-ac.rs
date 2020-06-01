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

pub struct UnionFind {
    nodes: Vec<UnionFindNode>,
}

enum UnionFindNode {
    Root { size: usize },
    Child { parent: RefCell<usize> },
}

impl UnionFind {
    /// Create an union find with `size` vertices.
    pub fn new(size: usize) -> Self {
        UnionFind {
            nodes: (0..size)
                .map(|_| UnionFindNode::Root { size: 1 })
                .collect::<Vec<_>>(),
        }
    }

    /// Get the root index and its component size.
    fn root_node(&self, v: usize) -> (usize, usize) {
        match &self.nodes[v] {
            &UnionFindNode::Root { size } => (v, size),
            &UnionFindNode::Child { parent: ref p } => {
                let (u, size) = self.root_node(*p.borrow());
                // Path compression.
                *p.borrow_mut() = u;
                (u, size)
            }
        }
    }

    /// Get the root index.
    pub fn root(&self, v: usize) -> usize {
        self.root_node(v).0
    }

    /// Get the size of component.
    pub fn size(&self, v: usize) -> usize {
        self.root_node(v).1
    }

    /// Determine if two vertices are in the same component.
    pub fn connects(&self, u: usize, v: usize) -> bool {
        self.root(u) == self.root(v)
    }

    /// Merge components by adding an edge between the two vertices.
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
    let N = read!(usize);
    let T = read![i64, i64; N];

    let mut x = (0..N)
        .map(|i| {
            let (x, _) = T[i];
            (x, i)
        })
        .collect::<Vec<_>>();
    x.sort();
    let dx = x.windows(2).map(|w| {
        let (x1, i1) = w[0];
        let (x2, i2) = w[1];
        (x2 - x1, i1, i2)
    });

    let mut y = (0..N)
        .map(|i| {
            let (_, y) = T[i];
            (y, i)
        })
        .collect::<Vec<_>>();
    y.sort();
    let dy = y.windows(2).map(|w| {
        let (y1, i1) = w[0];
        let (y2, i2) = w[1];
        (y2 - y1, i1, i2)
    });

    let mut edges = dx.chain(dy).collect::<Vec<_>>();
    edges.sort();

    let mut uf = UnionFind::new(N);
    let mut total = 0;

    for (d, i, j) in edges {
        if uf.connects(i, j) {
            continue;
        }

        total += d;
        uf.merge(i, j);
    }

    println!("{}", total)
}
