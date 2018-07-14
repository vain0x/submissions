#![allow(unused_imports)]
#![allow(non_snake_case)]

use std::cmp::{max, min, Ordering};
use std::collections::*;
use std::fmt::{Debug, Formatter};
use std::io::*;
use std::ops::*;
use std::*;

// -----------------------------------------------
// Framework
// -----------------------------------------------

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

#[allow(unused_macros)]
macro_rules! debug {
    ($($arg:expr),*) => {
        #[cfg(debug_assertions)]
        {
            let entries = [$(&stringify!([$arg]:), &$arg as &Debug),*];
            stderr().write_fmt(format_args!("{:#?}\n", entries)).unwrap();
        }
    };
}

#[allow(dead_code)]
fn rl() -> String {
    let mut buf = String::new();
    io::stdin().read_line(&mut buf).unwrap();
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

struct BruteForce {
    S: Vec<char>,
    si: usize,
    R: Vec<char>,
    B: Vec<char>,
    sols: HashMap<(Vec<char>, Vec<char>), usize>,
}

impl BruteForce {
    fn next(&mut self) {
        if self.si == self.S.len() {
            let pair = (self.R.clone(), self.B.clone());
            *self.sols.entry(pair).or_insert(0) += 1;
            return;
        }

        let c = self.S[self.si];
        self.si += 1;

        self.R.push(c);
        self.next();
        self.R.pop();

        self.B.push(c);
        self.next();
        self.B.pop();

        self.si -= 1;
    }

    pub fn run(S: Vec<char>) -> HashMap<(Vec<char>, Vec<char>), usize> {
        let mut solver = BruteForce {
            S: S,
            si: 0,
            R: Vec::new(),
            B: Vec::new(),
            sols: HashMap::new(),
        };
        solver.next();
        solver.sols
    }
}

pub fn main() {
    let N = read!(usize);
    let S = rl().chars().vec();

    let L = S.iter().take(N).cloned().vec();
    let R = S.iter().rev().take(N).cloned().vec();

    let ls = BruteForce::run(L);
    let rs = BruteForce::run(R);

    let mut total = 0;
    for ((r, b), lm) in ls {
        if let Some(&rm) = rs.get(&(r, b)) {
            total += lm * rm;
        }
    }

    println!("{}", total);
}
