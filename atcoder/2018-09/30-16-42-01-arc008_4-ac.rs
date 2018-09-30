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

#[derive(Clone, Debug)]
pub struct BucketVec<T, F> {
    node: Vec<T>,
    bucket: Vec<(usize, usize, T)>,
    bucket_size: usize,
    mempty: T,
    mappend: F,
}

impl<T, F> BucketVec<T, F>
where
    T: Clone,
    F: Fn(T, T) -> T,
{
    pub fn new(node: Vec<T>, mempty: T, mappend: F) -> BucketVec<T, F> {
        let bucket_size = (node.len() as f64).sqrt() as usize + 1;

        let mut bucket = vec![];
        let mut l = 0;
        let mut r = 1;
        while r <= node.len() {
            if r == node.len() || (r % bucket_size == 0) {
                bucket.push((l, r, mempty.clone()));
                l = r;
            }
            r += 1;
        }

        let mut it = BucketVec {
            node: node,
            bucket: bucket,
            bucket_size: bucket_size,
            mempty: mempty,
            mappend: mappend,
        };
        for i in 0..it.bucket.len() {
            it.refresh(i);
        }
        it
    }

    fn mempty(&self) -> T {
        self.mempty.clone()
    }

    fn mappend_mut(&self, l: &mut T, r: T) {
        let mut t = self.mempty();
        swap(l, &mut t);
        *l = (self.mappend)(t, r);
    }

    fn refresh(&mut self, i: usize) {
        let bi = i / self.bucket_size;
        let (bl, br, _) = self.bucket[bi];
        let mut sum = self.mempty();
        for i in bl..br {
            self.mappend_mut(&mut sum, self.node[i].clone());
        }
        self.bucket[bi] = (bl, br, sum);
    }

    pub fn sum(&self, ql: usize, qr: usize) -> T {
        let lbi = ql / self.bucket_size;
        let rbi = (qr + self.bucket_size - 1) / self.bucket_size;
        let mut sum = self.mempty();
        for bi in lbi..rbi {
            let &(bl, br, ref acc) = &self.bucket[bi];

            let l = if bl <= ql && ql < br { ql } else { bl };
            let r = if bl < qr && qr <= br { qr } else { br };

            if l == bl && r == br {
                self.mappend_mut(&mut sum, acc.clone());
            } else {
                let mut acc = self.mempty();
                for i in l..r {
                    self.mappend_mut(&mut acc, self.node[i].clone());
                }
                self.mappend_mut(&mut sum, acc);
            }
        }
        sum
    }

    pub fn set(&mut self, i: usize, item: T) {
        self.node[i] = item;
        self.refresh(i);
    }
}

// -----------------------------------------------
// Solution
// -----------------------------------------------

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

    let id = (1.0, 0.0);

    let mut tree = BucketVec::new(vec![id; N], id, |(la, lb), (ra, rb)| {
        (ra * la, rb + ra * lb)
    });

    let mut mi = OrdAdapter(1.0);
    let mut ma = OrdAdapter(1.0);
    for qi in 0..M {
        let (pi, pa, pb) = Q[qi];
        tree.set(pi, (pa, pb));

        let (a, b) = tree.sum(0, N);
        let v = OrdAdapter(a + b);

        mi = min(mi, v);
        ma = max(ma, v);
    }

    println!("{}", mi.0);
    println!("{}", ma.0);
}
