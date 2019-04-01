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

enum Status {
    Left,
    Right,
    Live,
}

struct Solver {
    S: Vec<char>,
    T: Vec<(char, char)>,
}

impl Solver {
    fn simulate(&self, mut i: usize) -> Status {
        for &(t, d) in &self.T {
            if self.S[i] != t {
                continue;
            }
            if d == 'L' && i == 0 {
                debug!((i, "left"));
                return Status::Left;
            }
            if d == 'L' {
                i -= 1;
            }
            if d == 'R' && i + 1 == self.S.len() {
                debug!((i, "right"));
                return Status::Right;
            }
            if d == 'R' {
                i += 1;
            }
        }
        debug!((i, "live"));
        Status::Live
    }

    fn solve(&self) -> usize {
        let N = self.S.len();

        // 消えるゴーレムの個数
        let mut k = 0;

        {
            // 0..l のゴーレムが全て左に消える

            let mut l = 0; // ok
            let mut r = N; // ng
            while r - l > 1 {
                let m = (l + r) / 2;

                let ok = match self.simulate(m - 1) {
                    Status::Left => true,
                    _ => false,
                };
                if ok {
                    l = m;
                } else {
                    r = m;
                }
            }

            k += l;
        }

        {
            // r..N のゴーレムがすべて右に消える

            let mut l = 0; // ng
            let mut r = N; // ok
            while r - l > 1 {
                let m = (l + r) / 2;

                let ok = match self.simulate(m) {
                    Status::Right => true,
                    _ => false,
                };
                if ok {
                    r = m;
                } else {
                    l = m;
                }
            }

            k += N - r;
        }

        N - k
    }
}

fn main() {
    let (_, Q) = read!(usize, usize);
    let S = rl().chars().collect::<Vec<_>>();
    let T = read!(String, String; Q)
        .into_iter()
        .map(|(t, d)| (t.chars().next().unwrap(), d.chars().next().unwrap()))
        .collect::<Vec<_>>();

    println!("{}", Solver { S: S, T: T }.solve())
}
