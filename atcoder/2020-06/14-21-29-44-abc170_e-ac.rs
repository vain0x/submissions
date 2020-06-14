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

#[derive(PartialEq, Clone, Debug)]
struct Interval<T> {
    l: T,
    r: T,
}

impl<T: Ord> Interval<T> {
    fn new(l: T, r: T) -> Interval<T> {
        Interval { l, r }
    }

    fn disjoint(&self, other: &Self) -> bool {
        self.r <= other.l || other.r <= self.l
    }

    fn covers(&self, other: &Self) -> bool {
        self.l <= other.l && other.r <= self.r
    }
}

#[derive(Debug)]
pub struct SegTree<T, F> {
    len: usize,

    /// Number of leaf nodes.
    width: usize,

    /// len = `2w-1` for `w-1` inners and `w` leaves,
    /// where `w` is the smallest `2^p` (`>= len`).
    node: Vec<T>,

    mempty: T,
    mappend: F,
}

impl<T, F> SegTree<T, F>
where
    T: Clone,
    F: Fn(T, T) -> T,
{
    pub fn new(items: Vec<T>, mempty: T, mappend: F) -> SegTree<T, F> {
        let len = items.len();

        let mut w = 1;
        while w < len {
            w *= 2;
        }
        debug_assert!(w >= len);

        let mut node = vec![mempty.clone(); 2 * w - 1];

        for (ei, item) in items.into_iter().enumerate() {
            node[w - 1 + ei] = item;
        }

        for ni in (0..w - 1).rev() {
            node[ni] = mappend(node[2 * ni + 1].clone(), node[2 * ni + 2].clone());
        }

        SegTree {
            len: len,
            width: w,
            node: node,
            mempty: mempty,
            mappend: mappend,
        }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn set(&mut self, ei: usize, value: T) {
        let mut ni = self.width - 1 + ei;
        self.node[ni] = value;

        while ni > 0 {
            ni = (ni - 1) / 2;
            self.node[ni] =
                (self.mappend)(self.node[2 * ni + 1].clone(), self.node[2 * ni + 2].clone());
        }
    }

    pub fn sum(&self, ql: usize, qr: usize) -> T {
        let q = Interval::new(ql, qr);
        if q.disjoint(&Interval::new(0, self.len())) {
            self.mempty.clone()
        } else {
            self.sum_core(0, Interval::new(0, self.width), &q)
        }
    }

    fn sum_core(&self, ni: usize, e: Interval<usize>, q: &Interval<usize>) -> T {
        if e.disjoint(&q) {
            self.mempty.clone()
        } else if q.covers(&e) {
            self.node[ni].clone()
        } else {
            let m = (e.l + e.r) / 2;
            let vl = self.sum_core(2 * ni + 1, Interval::new(e.l, m), q);
            let vr = self.sum_core(2 * ni + 2, Interval::new(m, e.r), q);
            (self.mappend)(vl.clone(), vr.clone())
        }
    }
}

const INF: usize = 1 << 60;
const M: usize = 2 * 100_000 + 1;

fn main() {
    let mut scan = Scan::new();

    let N = scan.word::<usize>();
    let Q = scan.word::<usize>();
    let mut A = vec![0; N];
    let mut B = vec![0; N];
    let mut C = vec![0; Q];
    let mut D = vec![0; Q];
    for i in 0..N {
        A[i] = scan.word::<usize>();
        B[i] = scan.word::<usize>() - 1;
    }
    for q in 0..Q {
        C[q] = scan.word::<usize>() - 1;
        D[q] = scan.word::<usize>() - 1;
    }

    let mut bn = B.clone();
    bn.sort();
    bn.dedup();

    let mut qs = vec![BinaryHeap::new(); M];
    let mut cancels = vec![HashSet::new(); M];
    let mut items = vec![INF; M];

    for i in 0..N {
        qs[B[i]].push((A[i], i));
    }

    for &bi in &bn {
        items[bi] = match qs[bi].peek() {
            Some(&(a, _)) => a,
            None => INF,
        };
    }

    let mut seg = SegTree::new(items, INF, std::cmp::min);

    for q in 0..Q {
        let c = C[q];
        let d = D[q];
        let b = B[c];

        cancels[b].insert(c);

        if cancels[d].contains(&c) {
            cancels[d].remove(&c);
        } else {
            qs[d].push((A[c], c));
        }

        B[c] = d;

        for &j in &[b, d] {
            let mut rate = INF;

            while let Some(&(a, c)) = qs[j].peek() {
                if cancels[j].contains(&c) {
                    cancels[j].remove(&c);
                    qs[j].pop();
                    continue;
                }
                rate = a;
                break;
            }

            seg.set(j, rate);
        }

        println!("{}", seg.sum(0, seg.len()));
    }
}
