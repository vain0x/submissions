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

const P: i64 = 1_000_000_007;

struct Solver {
    N: usize,
    M: usize,
    A: Vec<usize>,
    memo: Vec<Vec<i64>>,
}

impl Solver {
    fn dfs(&mut self, i: usize, dup: bool) -> i64 {
        let d = if dup { 1 } else { 0 };
        if self.memo[i][d] == P {
            let n = self.do_dfs(i, dup);
            self.memo[i][d] = n;
        }
        self.memo[i][d]
    }

    fn do_dfs(&mut self, i: usize, dup: bool) -> i64 {
        if i >= self.M {
            return if dup { 1 } else { 0 };
        }

        let a = self.A[i] as i64;
        let mut sum = 0;
        for c in 0..min(3, a + 1) {
            if c == 0 {
                sum += self.dfs(i + 1, dup);
                sum %= P;
            } else if c == 1 {
                sum += a * self.dfs(i + 1, dup) % P;
                sum %= P;
            } else {
                // C(a, 2) + C(a, 3) + .. + C(a, a) = 2^a - C(a, 1) - C(a, 0) = 2^a - a - 1
                let mut m = 1;
                for _ in 0..a {
                    m *= 2;
                    m %= P;
                }
                m += P - a - 1;
                m %= P;

                sum += m * self.dfs(i + 1, true) % P;
                sum %= P;
            }
        }

        sum
    }

    fn solve(&mut self) -> i64 {
        // 2^N - 1
        let mut n = 1_i64;
        for _ in 0..self.N {
            n *= 2;
            n %= P;
        }
        n += P - 1;
        n %= P;

        n += P - self.dfs(0, false);
        n %= P;

        n
    }
}

fn main() {
    let N = read!(usize);
    let S = rl();

    let mut A = BTreeMap::new();
    for c in S.chars() {
        *A.entry(c).or_insert(0) += 1_usize;
    }
    let A = A.values().cloned().collect::<Vec<_>>();
    let M = A.len();

    println!(
        "{}",
        Solver {
            N: N,
            M: M,
            A: A,
            memo: vec![vec![P, P]; M + 1]
        }
        .solve()
    )
}
