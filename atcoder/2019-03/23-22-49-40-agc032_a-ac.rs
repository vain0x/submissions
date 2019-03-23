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

fn check() {
    fn dfs(i: usize, B: &mut Vec<usize>) {
        if i >= B.len() {
            match (solve_by_force(&B), solve_fast(&B)) {
                (None, None) => {}
                (Some(_), Some(ref C)) if validate(C, &B) => {}
                (expected, actual) => {
                    debug!(B, expected, actual);
                    if let Some(ref C) = actual {
                        debug!(validate(C, &B));
                    }
                }
            }
            return;
        }

        for b in 0..B.len() {
            B[i] = b;
            dfs(i + 1, B);
        }
    }

    for n in 1..10 {
        debug!(n);
        let mut B = vec![0; n];
        dfs(0, &mut B);
    }

    debug!("success")
}

fn validate(C: &[usize], B: &[usize]) -> bool {
    assert_eq!(C.len(), B.len());
    let mut A = vec![];
    for i in 0..C.len() {
        if C[i] >= A.len() + 1 {
            return false;
        }
        A.insert(C[i], C[i]);
    }

    A == B
}

fn solve_by_force(B: &[usize]) -> Option<Vec<usize>> {
    fn dfs(i: usize, C: &mut Vec<usize>, B: &[usize]) -> Option<Vec<usize>> {
        if i >= B.len() {
            return if validate(C, B) {
                Some(C.to_owned())
            } else {
                None
            };
        }

        for c in 0..B.len() {
            C[i] = c;
            if let Some(C) = dfs(i + 1, C, B) {
                return Some(C);
            }
        }

        None
    }

    let mut C = B.to_owned();
    dfs(0, &mut C, B)
}

fn solve_fast(B: &[usize]) -> Option<Vec<usize>> {
    let N = B.len();

    let mut ok = true;
    let mut done = vec![false; N];
    let mut C = vec![];
    for _ in 0..N {
        let mut k = 0;
        let mut last_k = 0;
        let mut last_i = N;

        for i in 0..N {
            if done[i] {
                continue;
            }

            if B[i] == k {
                last_i = i;
                last_k = k;
            }

            k += 1;
        }

        if last_i >= N {
            ok = false;
            break;
        }

        done[last_i] = true;
        C.push(last_k);
    }

    if !ok {
        return None;
    }

    C.reverse();
    Some(C)
}

fn do_main() {
    read!(usize);
    let mut B = read![[usize]];
    let N = B.len();

    for i in 0..N {
        B[i] -= 1;
    }

    match solve_fast(&B) {
        None => println!("-1"),
        Some(C) => {
            for c in C {
                println!("{}", c + 1)
            }
        }
    }
}

fn main() {
    // check();
    do_main();
}
