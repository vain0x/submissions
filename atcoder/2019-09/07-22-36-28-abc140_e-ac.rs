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

#[derive(PartialEq, Clone, Debug)]
struct Interval<T> {
    l: T,
    r: T,
}

impl<T: Ord> Interval<T> {
    fn new(l: T, r: T) -> Interval<T> {
        Interval { l: l, r: r }
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

fn main() {
    let N = read!(usize);
    let P = read![[usize]];

    let mut lefts = SegTree::new(vec![(None, None); N], (None, None), |(l1, l2), (r1, r2)| {
        let mut x = vec![l1, l2, r1, r2];
        x.sort();
        (x[2], x[3])
    });

    let mut rights = SegTree::new(vec![(None, None); N], (None, None), |(l1, l2), (r1, r2)| {
        let mut x = vec![l1, l2, r1, r2];
        x.sort();
        for i in 0..x.len() {
            match x[i] {
                None => continue,
                Some(_) => {
                    if i + 1 < x.len() {
                        return (x[i], x[i + 1]);
                    }
                    return (None, x[i]);
                }
            }
        }
        (None, None)
    });

    let mut pos = vec![0; N + 1];
    for i in 0..N {
        pos[P[i]] = i;
    }

    let mut sum = 0_usize;

    for p in (1..N + 1).rev() {
        let i = pos[p];

        // 左に自分より大きい値がちょうど1回出現する区間の個数
        let first = {
            let (l1, l2) = lefts.sum(0, i);
            let left = match (l1, l2) {
                (None, None) => 0,
                (None, Some(l)) => l + 1,
                (Some(l1), Some(l2)) => l2 - l1,
                (Some(_), None) => unreachable!(),
            };
            let (r1, r2) = rights.sum(i + 1, N);
            let right = match (r1, r2) {
                (None, None) => N - i,
                (None, Some(r)) => r - i,
                (Some(r), _) => r - i,
            };
            debug!((p, left, right));
            left * right
        };

        // 右に自分より大きい値がちょうど1回出現する区間の個数
        let second = {
            let (l1, l2) = lefts.sum(0, i);
            let left = match (l1, l2) {
                (None, None) => i + 1,
                (None, Some(l)) | (Some(_), Some(l)) => i - l,
                (Some(_), None) => unreachable!(),
            };
            let (r1, r2) = rights.sum(i + 1, N);
            let right = match (r1, r2) {
                (None, None) => 0,
                (None, Some(r)) => N - r,
                (Some(r1), Some(r2)) => r2 - r1,
                (Some(_), None) => unreachable!(),
            };
            debug!((p, left, right));
            left * right
        };

        let count = first + second;
        // 長さ1の区間を除外
        // let count = count - min(1, count);

        sum += count * p;

        lefts.set(i, (None, Some(i)));
        rights.set(i, (None, Some(i)));
    }

    println!("{}", sum)
}
