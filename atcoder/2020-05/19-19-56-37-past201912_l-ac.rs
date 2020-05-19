//! Framework <https://github.com/vain0x/procon>

#![allow(non_snake_case)]
#![allow(unused_imports)]

use std::cmp::{max, min, Ordering};
use std::collections::*;
use std::fmt::{Debug, Display, Formatter, Write as FmtWrite};
use std::io::{stderr, stdin, BufRead, Write};

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

    #[allow(deprecated)]
    buf.trim_right().to_owned()
}

// -----------------------------------------------
// Solution
// -----------------------------------------------

use std::cell::RefCell;

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

#[derive(PartialEq, PartialOrd, Clone, Copy, Debug)]
pub struct OrdAdapter<T>(pub T);

impl<T: PartialEq> Eq for OrdAdapter<T> {}

impl<T: PartialOrd> Ord for OrdAdapter<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

fn sq(x: f64) -> f64 {
    x * x
}

fn weight(p: (f64, f64, i32), q: (f64, f64, i32)) -> f64 {
    let (x1, y1, c1) = p;
    let (x2, y2, c2) = q;

    let d = (sq(x1 - x2) + sq(y1 - y2)).sqrt();
    if c1 == c2 {
        d
    } else {
        d * 10.0
    }
}

fn main() {
    let (N, M) = read!(usize, usize);
    let U = read![f64, f64, i32; N];
    let mut T = read![f64, f64, i32; M];

    T.extend(U);

    let mut done = vec![];
    let mut edges = vec![];
    let mut min_cost = OrdAdapter(1e9);

    for s in 0..1 << M {
        let mut uf = UnionFind::new(M + N);

        let mut total = 0.0;

        done.clear();
        done.resize(M + N, false);

        edges.clear();
        for u in 0..M + N {
            if u < M && ((s >> u) & 1) == 0 {
                continue;
            }

            for v in u + 1..M + N {
                if v < M && ((s >> v) & 1) == 0 {
                    continue;
                }

                edges.push((weight(T[u], T[v]), u, v));
            }
        }

        edges.sort_by(|l, r| OrdAdapter(l).cmp(&OrdAdapter(r)));

        for (w, u, v) in edges.drain(..) {
            if uf.connects(u, v) {
                continue;
            }

            uf.merge(u, v);
            total += w;
        }

        min_cost = min(min_cost, OrdAdapter(total));
    }

    println!("{}", min_cost.0)
}
