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

/*
9 3 4
5 2
ULLR
RDDD
*/

struct Solver {
    N: usize,
    W: usize,
    R: char,
    L: char,
    S: Vec<char>,
    T: Vec<char>,
    dp_takahashi: Vec<Vec<Option<bool>>>,
    dp_aoki: Vec<Vec<Option<bool>>>,
}

impl Solver {
    fn takahashi(&mut self, i: usize, x: usize) -> bool {
        if let Some(ok) = self.dp_takahashi[i][x] {
            return ok;
        }

        let ok = self.do_takahashi(i, x);
        self.dp_takahashi[i][x] = Some(ok);
        ok
    }

    fn aoki(&mut self, i: usize, x: usize) -> bool {
        if let Some(ok) = self.dp_aoki[i][x] {
            return ok;
        }

        let ok = self.do_aoki(i, x);
        self.dp_aoki[i][x] = Some(ok);
        ok
    }

    fn do_takahashi(&mut self, i: usize, x: usize) -> bool {
        if i == self.N {
            return true;
        }

        if self.S[i] == self.L {
            if x == 1 {
                return false;
            }

            self.aoki(i, x - 1) && self.aoki(i, x)
        } else if self.S[i] == self.R {
            if x == self.W {
                return false;
            }

            self.aoki(i, x) && self.aoki(i, x + 1)
        } else {
            self.aoki(i, x)
        }
    }

    fn do_aoki(&mut self, i: usize, x: usize) -> bool {
        if i == self.N {
            return true;
        }

        if x >= 2 && self.T[i] == self.L {
            self.takahashi(i + 1, x - 1) || self.takahashi(i + 1, x)
        } else if x + 1 <= self.W && self.T[i] == self.R {
            self.takahashi(i + 1, x) || self.takahashi(i + 1, x + 1)
        } else {
            self.takahashi(i + 1, x)
        }
    }
}

fn main() {
    let (H, W, N) = read!(usize, usize, usize);
    let (Y, X) = read!(usize, usize);
    let S = rl().chars().collect::<Vec<_>>();
    let T = rl().chars().collect::<Vec<_>>();

    let mut ok = true;

    for &(X, W, R, L) in &[(X, W, 'R', 'L'), (Y, H, 'D', 'U')] {
        let dp_takahashi = vec![vec![None; W + 2]; N + 1];
        let dp_aoki = vec![vec![None; W + 2]; N + 1];
        ok = ok
            && Solver {
                N: N,
                W: W,
                L: L,
                R: R,
                S: S.clone(),
                T: T.clone(),
                dp_takahashi: dp_takahashi,
                dp_aoki: dp_aoki,
            }
            .takahashi(0, X);
    }

    println!("{}", if ok { "YES" } else { "NO" })
}
