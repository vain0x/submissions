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
    Child { parent: std::cell::Cell<usize> },
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
                let (u, size) = self.root_node(p.get());
                // Path compression.
                p.set(u);
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
            parent: std::cell::Cell::new(u),
        };
        self.nodes[u] = UnionFindNode::Root {
            size: u_size + v_size,
        };
    }
}

#[macro_export]
macro_rules! grid_vec {
    ($item:expr; $h:expr, $w:expr) => {{
        let item = $item;
        let mut it = GridVec::new();
        it.resize($h, $w, item);
        it
    }};
}

#[derive(Clone)]
pub struct GridVec<T> {
    h: usize,
    w: usize,
    data: Vec<T>,
}

impl<T> GridVec<T> {
    pub fn new() -> Self {
        GridVec {
            h: 0,
            w: 0,
            data: Vec::new(),
        }
    }

    pub fn clear(&mut self) {
        self.h = 0;
        self.w = 0;
        self.data.clear();
    }

    pub fn resize(&mut self, h: usize, w: usize, item: T)
    where
        T: Clone,
    {
        self.h = h;
        self.w = w;
        self.data.clear();
        self.data.resize(h * w, item);
    }

    pub fn push_row<IntoIter: IntoIterator<Item = T>>(&mut self, row: IntoIter)
    where
        T: Clone + Default,
    {
        if self.h == 0 {
            self.data.extend(row);
            self.h = 1;
            self.w = self.data.len();
        } else {
            self.h += 1;
            let w = self.w;
            self.data.extend(row.into_iter().take(w));
            self.data.resize(self.h * w, T::default());
        }
    }

    pub fn get(&self, index: (usize, usize)) -> Option<&T> {
        let i = index.0 * self.w + index.1;
        self.data.get(i)
    }

    pub fn get_mut(&mut self, index: (usize, usize)) -> Option<&mut T> {
        let i = index.0 * self.w + index.1;
        self.data.get_mut(i)
    }

    pub fn enumerate<'a>(&'a self) -> GridEnumerateIter<'a, T> {
        // '
        GridEnumerateIter {
            owner: self,
            y: 0,
            x: 0,
            i: 0,
        }
    }

    pub fn neighbor(
        &self,
        index: (usize, usize),
        offset: (isize, isize),
    ) -> Option<(usize, usize)> {
        let y = (index.0 as isize + offset.0) as usize;
        let x = (index.1 as isize + offset.1) as usize;
        if y < self.h && x < self.w {
            Some((y, x))
        } else {
            None
        }
    }

    pub fn four_neighbors(&self, index: (usize, usize)) -> Vec<(usize, usize)> {
        [(0, -1), (0, 1), (-1, 0), (1, 0)]
            .iter()
            .filter_map(|&offset| self.neighbor(index, offset))
            .collect()
    }
}

pub struct GridEnumerateIter<'a, T: 'a> {
    owner: &'a GridVec<T>, // '
    y: usize,
    x: usize,
    i: usize,
}

impl<'a, T> Iterator for GridEnumerateIter<'a, T> {
    type Item = ((usize, usize), &'a T); // '

    fn next(&mut self) -> Option<Self::Item> {
        let (y, x, i) = (self.y, self.x, self.i);
        if y == self.owner.h {
            return None;
        }

        self.i += 1;
        self.x += 1;
        if self.x == self.owner.w {
            self.x = 0;
            self.y += 1;
        }

        let item = unsafe { self.owner.data.get_unchecked(i) };
        Some(((y, x), item))
    }
}

impl<T> std::ops::Index<(usize, usize)> for GridVec<T> {
    type Output = T;

    fn index(&self, index: (usize, usize)) -> &T {
        let i = index.0 * self.w + index.1;
        &self.data[i]
    }
}

impl<T> std::ops::IndexMut<(usize, usize)> for GridVec<T> {
    fn index_mut(&mut self, index: (usize, usize)) -> &mut T {
        let i = index.0 * self.w + index.1;
        &mut self.data[i]
    }
}

const INF: usize = 1 << 60;

fn main() {
    let mut scan = Scan::new();

    let H = scan.word::<usize>();
    let W = scan.word::<usize>();
    let K = scan.word::<usize>();
    let y1 = scan.word::<usize>() - 1;
    let x1 = scan.word::<usize>() - 1;
    let y2 = scan.word::<usize>() - 1;
    let x2 = scan.word::<usize>() - 1;

    let s = (y1, x1);
    let t = (y2, x2);

    let mut board = GridVec::new();
    for row in scan.list::<String>(H) {
        board.push_row(row.chars());
    }

    let enc = |(y, x)| y * W + x;

    let mut uf = UnionFind::new(H * W);
    for y in 0..H {
        for x in 0..W {
            let u = (y, x);
            if board[u] == '@' {
                continue;
            }

            for v in board.four_neighbors(u) {
                if board[v] == '@' {
                    continue;
                }

                uf.merge(enc(u), enc(v));
            }
        }
    }

    if !uf.connects(enc(s), enc(t)) {
        println!("-1");
        return;
    }

    let mut dist = GridVec::new();
    dist.resize(H, W, INF);

    let mut queue = VecDeque::new();
    dist[s] = 0;
    queue.push_back((s, 0));

    'a: while let Some((u, d)) = queue.pop_front() {
        if dist[u] != d {
            continue;
        }

        for &(dy, dx) in &[(0, -1), (0, 1), (-1, 0), (1, 0)] {
            for k in 1..=K as isize {
                let v = match board.neighbor(u, (dy * k, dx * k)) {
                    Some(v) if board[v] == '@' => break,
                    Some(v) => v,
                    None => break,
                };

                if dist[v] == d + 1 {
                    continue;
                }
                if dist[v] <= d {
                    break;
                }

                dist[v] = d + 1;
                queue.push_back((v, d + 1));

                if v == (y2, x2) {
                    break 'a;
                }
            }
        }
    }

    println!("{}", dist[(y2, x2)]);
}
