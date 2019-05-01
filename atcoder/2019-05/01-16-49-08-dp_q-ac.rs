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
            it.refresh(i * bucket_size);
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
    let N = read!(usize);
    let H = read![[usize]];
    let A = read![[i64]];

    // S[h] = x : 高さの最大値が h になるような選び方で最大の美しさの総和
    let mut S = BucketVec::new(vec![0; N + 2], 0, max);

    for i in 0..N {
        let h = H[i];
        let a = S.sum(0, h) + A[i];
        S.set(h, a);
    }

    println!("{}", S.sum(0, N + 1));
}
