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

#[derive(Clone, Copy)]
enum Query {
    Set { i: usize, c: char },
    Count { l: usize, r: usize },
}

fn char_to_index(c: char) -> usize {
    (c as u8 - b'a') as usize
}

fn char_to_bit(c: char) -> usize {
    1 << char_to_index(c)
}

struct Solver {
    N: usize,
    S: Vec<char>,
    queries: Vec<Query>,
    block_size: usize,
    block_l: Vec<usize>,
    block_r: Vec<usize>,
    block_bitset: Vec<usize>,
}

impl Solver {
    fn new(N: usize, S: String, queries: Vec<Query>) -> Solver {
        let S = S.chars().collect::<Vec<_>>();

        let block_size = min((N as f64).sqrt() as usize + 1, N);
        let block_count = (N + block_size - 1) / block_size;

        let block_l = vec![0; block_count];
        let block_r = vec![0; block_count];
        let block_bitset = vec![0; block_count];

        Solver {
            N: N,
            S: S,
            queries: queries,
            block_size: block_size,
            block_l: block_l,
            block_r: block_r,
            block_bitset: block_bitset,
        }
    }

    fn block_count(&self) -> usize {
        self.block_l.len()
    }

    fn refresh(&mut self, k: usize) {
        self.block_bitset[k] = 0;

        for i in self.block_l[k]..self.block_r[k] {
            let k = i / self.block_size;
            self.block_bitset[k] |= char_to_bit(self.S[i]);
        }
    }

    fn solve(&mut self) {
        for i in 0..self.block_count() {
            self.block_l[i] = i * self.block_size;
            self.block_r[i] = min((i + 1) * self.block_size, self.N);
        }

        for k in 0..self.block_count() {
            self.refresh(k);
        }

        for qi in 0..self.queries.len() {
            match self.queries[qi] {
                Query::Set { i, c } => {
                    if self.S[i] != c {
                        self.S[i] = c;

                        let k = i / self.block_size;
                        self.refresh(k);
                    }
                }
                Query::Count { l, r } => {
                    let mut bitset = 0;

                    let mut i = l;

                    while i < r {
                        let k = i / self.block_size;

                        if i == self.block_l[k] && self.block_r[k] <= r {
                            bitset |= self.block_bitset[k];
                            i = self.block_r[k];
                            continue;
                        }

                        bitset |= char_to_bit(self.S[i]);
                        i += 1;
                    }

                    println!("{}", bitset.count_ones());
                }
            }
        }
    }
}

fn main() {
    let N = read!(usize);
    let S = rl();
    let Q = read!(usize);

    let mut queries = vec![];

    for _ in 0..Q {
        let line = rl();
        let mut words = line.split_whitespace();

        let t = words.next().unwrap().parse::<i32>().unwrap();
        match t {
            1 => {
                let i = words.next().unwrap().parse::<usize>().unwrap() - 1;
                let c = words.next().unwrap().chars().next().unwrap();
                queries.push(Query::Set { i: i, c: c });
            }
            2 => {
                let l = words.next().unwrap().parse::<usize>().unwrap() - 1;
                let r = words.next().unwrap().parse::<usize>().unwrap();
                queries.push(Query::Count { l: l, r: r });
            }
            _ => unreachable!(),
        }
    }

    let mut solver = Solver::new(N, S, queries);
    solver.solve();
}
