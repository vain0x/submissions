#![allow(unused_imports)]
#![allow(non_snake_case)]

use std::cell::RefCell;
use std::cmp::{max, min, Ordering};
use std::collections::*;
use std::fmt::{Debug, Formatter, Write as FmtWrite};
use std::io::{stderr, stdin, BufRead, Write};
use std::mem::{replace, swap};
use std::ops::*;
use std::rc::Rc;

// -----------------------------------------------
// Framework <https://github.com/vain0x/procon>
// -----------------------------------------------

#[cfg(debug_assertions)]
include!{"./procon/debug.rs"}

#[cfg(not(debug_assertions))]
macro_rules! debug {
    ($($arg:expr),*) => {};
}

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

/// Wraps a partial-ord value to impl Ord.
#[derive(PartialEq, PartialOrd, Clone, Copy, Debug)]
pub struct OrdAdapter<T>(pub T);

impl<T: PartialEq> Eq for OrdAdapter<T> {}

impl<T: PartialOrd> Ord for OrdAdapter<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

// -----------------------------------------------
// Solution
// -----------------------------------------------

pub trait MonoidOp {
    type T;

    fn empty() -> Self::T;
    fn append(l: Self::T, r: Self::T) -> Self::T;
}

#[derive(PartialEq, Clone, Debug)]
struct Interval<T> {
    l: T,
    r: T,
}

impl<T: Ord> Interval<T> {
    fn new(l: T, r: T) -> Interval<T> {
        Interval { l: l, r: r }
    }

    fn empty(&self) -> bool {
        self.l >= self.r
    }

    fn disjoint(&self, other: &Self) -> bool {
        self.r <= other.l || other.r <= self.l
    }

    fn covers(&self, other: &Self) -> bool {
        self.l <= other.l && other.r <= self.r
    }
}

#[derive(Debug)]
pub struct SegTree<T, M> {
    len: usize,
    width: usize,

    /// len = `2w-1` for `w-1` inners and `w` leaves,
    /// where `w` is the smallest `2^p` (`>= len`).
    node: Vec<T>,

    monoidOp: std::marker::PhantomData<M>,
}

impl<T: Clone, M: MonoidOp<T = T>> SegTree<T, M> {
    pub fn new(items: Vec<T>) -> SegTree<T, M> {
        let len = items.len();

        let mut w = 1;
        while w < len {
            w *= 2;
        }
        debug_assert!(w >= len);

        let mut node = vec![M::empty(); 2 * w - 1];

        for (ei, item) in items.into_iter().enumerate() {
            node[w - 1 + ei] = item;
        }

        for ni in (0..w - 1).rev() {
            node[ni] = M::append(node[2 * ni + 1].clone(), node[2 * ni + 2].clone());
        }

        SegTree {
            len: len,
            width: w,
            node: node,
            monoidOp: std::marker::PhantomData,
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
            self.node[ni] = M::append(self.node[2 * ni + 1].clone(), self.node[2 * ni + 2].clone());
        }
    }

    pub fn sum(&self, ql: usize, qr: usize) -> T {
        let q = Interval::new(ql, qr);
        if q.empty() {
            M::empty()
        } else {
            self.sum_core(0, Interval::new(0, self.width), &q)
        }
    }

    fn sum_core(&self, ni: usize, e: Interval<usize>, q: &Interval<usize>) -> T {
        if e.disjoint(&q) {
            M::empty()
        } else if q.covers(&e) {
            self.node[ni].clone()
        } else {
            let m = (e.l + e.r) / 2;
            let vl = self.sum_core(2 * ni + 1, Interval::new(e.l, m), q);
            let vr = self.sum_core(2 * ni + 2, Interval::new(m, e.r), q);
            M::append(vl.clone(), vr.clone())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{MonoidOp, SegTree};
    use std;
    use std::cmp::min;
    use std::marker::PhantomData;

    #[derive(Debug)]
    struct Min<T>(PhantomData<T>);

    impl MonoidOp for Min<i64> {
        type T = i64;

        fn empty() -> Self::T {
            std::i64::MAX
        }

        fn append(l: Self::T, r: Self::T) -> Self::T {
            min(l, r)
        }
    }

    #[test]
    fn test_segtree_min() {
        let mut tree = SegTree::<_, Min<_>>::new(vec![10; 10]);

        assert_eq!(tree.sum(0, 10), 10);
        assert_eq!(tree.sum(10, 10), std::i64::MAX);

        tree.set(5, 5);
        assert_eq!(tree.sum(0, 5), 10);
        assert_eq!(tree.sum(5, 6), 5);
        assert_eq!(tree.sum(6, 10), 10);
        assert_eq!(tree.sum(4, 8), 5);
        assert_eq!(tree.sum(0, 10), 5);

        tree.set(3, 3);
        assert_eq!(tree.sum(0, 3), 10);
        assert_eq!(tree.sum(3, 5), 3);
        assert_eq!(tree.sum(5, 10), 5);
        assert_eq!(tree.sum(0, 10), 3);
    }
}

#[derive(Debug, Clone)]
struct Proc(f64, f64);

impl MonoidOp for Proc {
    type T = Proc;

    fn empty() -> Self {
        Proc(1.0, 0.0)
    }

    fn append(l: Proc, r: Proc) -> Self::T {
        let Proc(a1, b1) = l;
        let Proc(a2, b2) = r;
        Proc(a2 * a1, a2 * b1 + b2)
    }
}

fn main() {
    let (_, M) = read!(usize, usize);
    let Q = read![i64, f64, f64; M];

    // 座標圧縮
    let (N, Q) = {
        let mut P = Q.iter().map(|&(p, _, _)| p).vec();
        P.sort();
        P.dedup();

        let mut PInv = BTreeMap::new();
        for i in 0..P.len() {
            PInv.insert(P[i], i);
        }

        let N = P.len();
        let Q = Q.into_iter().map(|(p, a, b)| (PInv[&p], a, b)).vec();
        (N, Q)
    };

    let mut tree = SegTree::<_, Proc>::new(vec![Proc::empty(); N]);

    let mut mi = OrdAdapter(1.0);
    let mut ma = OrdAdapter(1.0);
    for qi in 0..M {
        let (pi, pa, pb) = Q[qi];
        tree.set(pi, Proc(pa, pb));

        let Proc(a, b) = tree.sum(0, N);
        let v = OrdAdapter(a + b);

        mi = min(mi, v);
        ma = max(ma, v);
    }

    println!("{}", mi.0);
    println!("{}", ma.0);
}
