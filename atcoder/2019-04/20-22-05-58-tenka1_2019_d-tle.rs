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

const P: i64 = 998244353;

struct Solver {
    A: Vec<i32>,
    S: i32,
    dp: BTreeMap<(usize, i32, i32), i64>,
}

impl Solver {
    fn dfs(&mut self, k: usize, r: i32, g: i32) -> i64 {
        if k >= self.A.len() {
            let b = self.S - (r + g);
            return if r + g > b && g + b > r && b + r > g {
                1
            } else {
                0
            };
        }

        if let Some(&n) = self.dp.get(&(k, r, g)) {
            return n;
        }

        let mut n = 0;
        let a = self.A[k];

        n += self.dfs(k + 1, r + a, g);
        n %= P;
        n += self.dfs(k + 1, r, g + a);
        n %= P;
        n += self.dfs(k + 1, r, g);
        n %= P;

        self.dp.insert((k, r, g), n);
        n
    }
}

fn main() {
    let N = read!(usize);
    let A = read![i32; N];
    let S = A.iter().sum::<i32>();

    let n = Solver {
        A: A,
        S: S,
        dp: BTreeMap::new(),
    }
    .dfs(0, 0, 0);

    println!("{}", n)
}
