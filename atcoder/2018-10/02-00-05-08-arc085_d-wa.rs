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

// -----------------------------------------------
// Solution
// -----------------------------------------------

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

        // Chunkify nodes into buckets. Bucket sums are inconsistent yet.
        let mut bucket = vec![];
        let mut l = 0;
        let mut r = 1;
        while r <= node.len() {
            if r == node.len() || r % bucket_size == 0 {
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
            it.refresh(i * bucket_size); // FIXED!!
        }

        it
    }

    fn mempty(&self) -> T {
        self.mempty.clone()
    }

    /// Does `*l (mpappend)= r` in an efficient way.
    fn mappend_mut(&self, l: &mut T, r: T) {
        let t = std::mem::replace(l, self.mempty());
        *l = (self.mappend)(t, r);
    }

    /// Recalculates the bucket sum of node `i`.
    fn refresh(&mut self, i: usize) {
        let bi = i / self.bucket_size;
        let (bl, br, _) = self.bucket[bi];
        let mut sum = self.mempty();
        for i in bl..br {
            self.mappend_mut(&mut sum, self.node[i].clone());
        }
        self.bucket[bi] = (bl, br, sum);
    }

    pub fn set(&mut self, i: usize, item: T) {
        self.node[i] = item;
        self.refresh(i);
    }

    /// Calculates sum of nodes `ql..qr`. O(√N).
    pub fn sum(&self, ql: usize, qr: usize) -> T {
        // Range of buckets that involves in the query.
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
                // If l or r isn't at boundary of buckets,
                // we need to sum up partially-covered nodes.

                let mut acc = self.mempty();
                for i in l..r {
                    self.mappend_mut(&mut acc, self.node[i].clone());
                }
                self.mappend_mut(&mut sum, acc);
            }
        }
        sum
    }
}

fn main() {
    read!(usize);
    let B = read![[i32]];
    let Q = read!(usize);

    let mut s = B.iter().cloned().sum::<i32>();
    debug!(s);

    let mut buckets = BucketVec::new(
        B.into_iter().map(|b| if b == 0 { -1 } else { 1 }).vec(),
        0,
        |l, r| l + r,
    );
    let bucket_size = buckets.bucket_size;

    debug!(buckets.bucket);

    let mut frozen = vec![false; buckets.bucket.len()];

    for (l, r) in read![usize, usize; Q] {
        let l = l - 1;

        let t = buckets.sum(l, r);
        debug!((l, r, t));

        if t > 0 {
            s -= t;

            let (ql, qr) = (l, r);
            // Range of buckets that involves in the query.
            let lbi = ql / bucket_size;
            let rbi = (qr + bucket_size - 1) / bucket_size;

            for bi in lbi..rbi {
                let (bl, br, _) = buckets.bucket[bi];

                let l = if bl <= ql && ql < br { ql } else { bl };
                let r = if bl < qr && qr <= br { qr } else { br };

                if l == bl && r == br {
                    buckets.bucket[bi] = (l, r, 0);
                    frozen[bi] = true;
                } else if !frozen[bi] {
                    // If l or r isn't at boundary of buckets,
                    // we need to sum up partially-covered nodes.

                    for i in l..r {
                        buckets.node[i] = 0;
                    }

                    buckets.refresh(l);
                }
            }
        }
    }

    println!("{}", s)
}
