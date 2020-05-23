//! Framework <https://github.com/vain0x/procon>

#![allow(non_snake_case)]
#![allow(unused_imports)]

use std::collections::*;

pub struct Scan(Box<dyn Iterator<Item = &'static str>>); // '

impl Scan {
    fn new() -> Self {
        let mut buf = String::new();
        let read_line = move || {
            std::io::stdin().read_line(&mut buf).unwrap();
            Box::leak(buf.split_off(0).into_boxed_str()).split_whitespace()
        };
        Scan(Box::new(std::iter::repeat_with(read_line).flatten()))
    }

    pub fn word<T: std::str::FromStr>(&mut self) -> T {
        self.0.next().unwrap().parse().ok().unwrap()
    }

    pub fn list<T: std::str::FromStr>(&mut self, len: usize) -> Vec<T> {
        std::iter::repeat_with(|| self.word()).take(len).collect()
    }
}

pub struct UnionFind {
    nodes: Vec<UnionFindNode>,
}

enum UnionFindNode {
    Root { size: usize },
    Child { parent: std::cell::RefCell<usize> },
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
            parent: std::cell::RefCell::new(u),
        };
        self.nodes[u] = UnionFindNode::Root {
            size: u_size + v_size,
        };
    }
}

fn main() {
    let mut scan = Scan::new();

    let N = scan.word::<usize>();
    let M = N * N;

    let P = scan.list::<usize>(M);

    let mut ord = vec![vec![]; N];
    let mut depth = vec![0; M];
    let mut u = 0;
    for y in 0..N {
        for x in 0..N {
            ord[y].push(u);
            u += 1;

            let d = y.min(x).min(N - 1 - y).min(N - 1 - x);
            depth[ord[y][x]] = d;
        }
    }

    let mut g = vec![vec![]; M];
    for y in 0..N {
        for x in 0..N {
            let u = ord[y][x];

            for &(dy, dx) in &[(1, 0), (0, -1), (-1, 0), (0, 1)] {
                let (y, x) = (y as isize + dy, x as isize + dx);
                if y < 0 || x < 0 || y >= N as isize || x >= N as isize {
                    continue;
                }
                let (y, x) = (y as usize, x as usize);

                let v = ord[y][x];
                g[u].push(v);
                g[v].push(u);
            }
        }
    }

    let mut uf = UnionFind::new(M);
    let mut done = vec![false; M];
    let mut q = VecDeque::new();
    let mut adjacents = vec![];
    let mut total = 0;

    for p in P {
        let u = p - 1;
        let mut d = depth[u];

        for i in 0..g[u].len() {
            let v = g[u][i];

            let mut t = depth[uf.root(v)];
            if !done[v] {
                t += 1;
            }

            d = d.min(t);
        }

        #[cfg(debug_assertions)]
        eprintln!("\x1B[33m{}\x1B[0m = {:?}", "(p, d)", (p, d));

        total += d;
        done[u] = true;

        let mut u = u;
        q.push_back(std::mem::take(&mut g[u]));

        while let Some(mut neighbors) = q.pop_front() {
            let v = match neighbors.pop() {
                Some(v) => {
                    q.push_back(neighbors);
                    v
                }
                None => continue,
            };

            if !done[v] {
                adjacents.push(v);
                d = d.min(depth[v] + 1);
                continue;
            }

            let v = uf.root(v);
            if v == u {
                continue;
            }

            q.push_back(std::mem::take(&mut g[v]));

            let d = depth[u].min(depth[v]);

            uf.merge(u, v);
            u = uf.root(u);

            depth[u] = d;
        }

        std::mem::swap(&mut g[u], &mut adjacents);
        adjacents.clear();

        #[cfg(debug_assertions)]
        {
            for y in 0..N {
                for x in 0..N {
                    let u = y * N + x;
                    let w = uf.root(u);
                    eprint!(" {:2}", depth[w]);
                }
                eprintln!("");
            }
        }
    }

    println!("{}", total);
}
