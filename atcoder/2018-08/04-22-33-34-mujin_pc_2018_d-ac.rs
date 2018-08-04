#![allow(unused_imports)]
#![allow(non_snake_case)]

use std::cell::RefCell;
use std::cmp::{max, min, Ordering};
use std::collections::*;
use std::fmt::{Debug, Formatter};
use std::io::*;
use std::mem::{replace, swap};
use std::ops::*;
use std::rc::Rc;

// -----------------------------------------------
// Framework
// -----------------------------------------------

#[allow(unused_macros)]
macro_rules! read {
    ([$t:ty] ; $n:expr) =>
        ((0..$n).map(|_| read!([$t])).collect::<Vec<_>>());
    ($($t:ty),+ ; $n:expr) =>
        ((0..$n).map(|_| read!($($t),+)).collect::<Vec<_>>());
    ([$t:ty]) =>
        (rl().split_whitespace().map(|w| w.parse().unwrap()).collect::<Vec<$t>>());
    ($t:ty) =>
        (rl().parse::<$t>().unwrap());
    ($($t:ty),*) => {{
        let buf = rl();
        let mut w = buf.split_whitespace();
        ($(w.next().unwrap().parse::<$t>().unwrap()),*)
    }};
}

#[allow(unused_macros)]
macro_rules! debug {
    ($($arg:expr),*) => {
        #[cfg(debug_assertions)]
        {
            let entries = [$(&stringify!([$arg]:), &$arg as &Debug),*];
            stderr().write_fmt(format_args!("{:?}\n", entries)).unwrap();
        }
    };
}

#[allow(dead_code)]
fn rl() -> String {
    let mut buf = String::new();
    stdin().read_line(&mut buf).unwrap();
    buf.trim_right().to_owned()
}

trait IteratorExt: Iterator + Sized {
    fn vec(self) -> Vec<Self::Item> {
        self.collect()
    }
}

impl<T: Iterator> IteratorExt for T {}

// -----------------------------------------------
// Solution
// -----------------------------------------------

#[derive(Debug)]
pub struct UnionFindForest {
    nodes: Vec<UffNode>,
}

#[derive(Debug)]
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

pub fn main() {
    const MAX: usize = 1000;

    let mut rev = vec![0; MAX];
    for n in 0..MAX {
        rev[n] = n
            .to_string()
            .chars()
            .rev()
            .collect::<String>()
            .parse::<usize>()
            .unwrap();
    }

    let f = |x, y| {
        let (x, y) = if x < y { (rev[x], y) } else { (x, rev[y]) };
        if x < y {
            (x, y - x)
        } else {
            (x - y, y)
        }
    };

    fn id(n: usize, m: usize) -> usize {
        n * MAX + m
    }

    let mut uff = UnionFindForest::new(MAX * MAX);

    for n in 0..MAX {
        for m in 0..MAX {
            let (n2, m2) = f(n, m);
            uff.merge(id(n, m), id(n2, m2));

            if n == 0 || m == 0 || n2 == 0 || m2 == 0 {
                uff.merge(id(n, m), id(0, 0));
            }
        }
    }

    let (N, M) = read!(usize, usize);

    let mut s = 0;
    for n in 1..N + 1 {
        for m in 1..M + 1 {
            if !uff.connects(id(0, 0), id(n, m)) {
                s += 1;
            }
        }
    }

    println!("{}", s);
    return;
}
