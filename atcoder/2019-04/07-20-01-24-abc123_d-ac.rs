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

struct Solver {
    K: usize,
    A: Vec<i64>,
    B: Vec<i64>,
    C: Vec<i64>,
    done: BTreeSet<(usize, usize, usize)>,
    heap: BinaryHeap<(i64, (usize, usize, usize))>,
}

impl Solver {
    fn push(&mut self, a: usize, b: usize, c: usize) {
        if a >= self.A.len() || b >= self.B.len() || c >= self.C.len() {
            return;
        }

        if self.done.insert((a, b, c)) {
            self.heap
                .push((self.A[a] + self.B[b] + self.C[c], (a, b, c)));
        }
    }

    fn solve(&mut self) {
        self.push(0, 0, 0);

        for _ in 0..self.K {
            let (sum, (a, b, c)) = self.heap.pop().unwrap();
            println!("{}", sum);

            self.push(a + 1, b, c);
            self.push(a, b + 1, c);
            self.push(a, b, c + 1);
        }
    }
}

fn main() {
    let (_, _, _, K) = read!(usize, usize, usize, usize);
    let mut A = read![[i64]];
    let mut B = read![[i64]];
    let mut C = read![[i64]];

    A.sort_by(|l, r| r.cmp(l));
    B.sort_by(|l, r| r.cmp(l));
    C.sort_by(|l, r| r.cmp(l));

    let done = BTreeSet::new();
    let heap = BinaryHeap::new();

    Solver {
        K: K,
        A: A,
        B: B,
        C: C,
        done: done,
        heap: heap,
    }
    .solve()
}
