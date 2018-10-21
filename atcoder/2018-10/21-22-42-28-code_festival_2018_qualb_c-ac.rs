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

// -----------------------------------------------
// Solution
// -----------------------------------------------

struct Solver {
    N: usize,
    k: usize,
    S: Vec<Vec<char>>,
}

impl Solver {
    fn solve(mut self) {
        let N = self.N;

        for y in 1..N + 1 {
            for x in 1..N + 1 {
                if (2 * y + x) % 5 == 0 {
                    self.put(y, x);
                }
            }
        }

        for y in 1..N + 1 {
            for x in 1..N + 1 {
                if self.is_edible(y, x) {
                    self.put(y, x);
                }
            }
        }

        self.output();
    }

    fn put(&mut self, y: usize, x: usize) {
        self.S[y][x] = 'X';
        self.k += 1;
    }

    fn output(&self) {
        let N = self.N;
        for y in 1..N + 1 {
            let line: String = self.S[y][1..N + 1].into_iter().cloned().collect();
            println!("{}", line);
        }
    }

    fn is_edible(&self, y: usize, x: usize) -> bool {
        let mut ok = true;
        for &(y, x) in &[(y, x), (y + 1, x), (y, x + 1), (y - 1, x), (y, x - 1)] {
            ok = ok && self.S[y][x] != 'X';
        }
        ok
    }

    // fn verify(&self) {
    //     let N = self.N;

    //     for y in 1..N + 1 {
    //         for x in 1..N + 1 {
    //             if self.is_edible(y, x) {
    //                 panic!("WA edible")
    //             }
    //         }
    //     }

    //     if self.k >= 201_800 {
    //         panic!("WA too many")
    //     }
    // }

    // fn rate(&self) -> f64 {
    //     (self.k as f64) / (self.N * self.N) as f64
    // }
}

fn main() {
    let N = read!(usize);
    let S = vec![vec!['.'; N + 2]; N + 2];

    let solver = Solver { N: N, S: S, k: 0 };
    solver.solve();
}
