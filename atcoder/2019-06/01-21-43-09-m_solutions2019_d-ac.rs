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

/// Rearranges elements in the slice
/// to the next lexicographically larger permutation.
pub fn next_perm<T: Ord>(xs: &mut [T]) -> bool {
    // `xs[i + 1..]` : desc but
    // `xs[i..]` : not desc.
    let i = match (0..xs.len())
        .rev()
        .filter(|&i| i + 1 < xs.len() && xs[i] < xs[i + 1])
        .next()
    {
        None => return false,
        Some(i) => i,
    };

    // `xs[k]` : The next greater elem in `xs[i..]`.
    let k = (i + 1..xs.len())
        .rev()
        .filter(|&k| xs[i] < xs[k])
        .next()
        .unwrap();

    // E.g. 2431 -> 3421 -> 3124 (where i = 0, k = 2).
    xs.swap(i, k);
    xs[i + 1..].reverse();

    true
}

// C[u] = c: ノード u に書かれた整数
fn calc(G: &[Vec<usize>], C: &[i64]) -> i64 {
    fn go(u: usize, p: usize, G: &[Vec<usize>], C: &[i64]) -> i64 {
        let mut sum = 0;

        for i in 0..G[u].len() {
            let v = G[u][i];
            if v == p {
                continue;
            }

            sum += min(C[u], C[v]);
            sum += go(v, u, G, C);
        }

        sum
    }

    go(0, G.len(), G, C)
}

fn brute_force(G: &[Vec<usize>], C: &[i64]) -> (i64, Vec<i64>) {
    let mut C = C.to_owned();
    let mut max_sum = 0;
    let mut max_C = vec![];
    while next_perm(&mut C) {
        let s = calc(G, &C);
        if max_sum < s {
            max_sum = s;
            max_C = C.clone();
        }
    }
    (max_sum, max_C)
}

fn solve(G: &[Vec<usize>], C: &[i64]) -> (i64, Vec<i64>) {
    let N = G.len();
    let mut deg = vec![0; N];
    for u in 0..N {
        for i in 0..G[u].len() {
            let v = G[u][i];
            deg[u] += 1;
            deg[v] += 1;
        }
    }

    let root = (0..N).max_by(|&l, &r| deg[r].cmp(&deg[l])).unwrap();
    let mut queue = VecDeque::new();
    queue.push_back((root, N));

    let mut C = C.to_owned();
    C.sort();

    let mut W = vec![0; N];

    while let Some((u, p)) = queue.pop_front() {
        W[u] = C.pop().unwrap();

        for i in 0..G[u].len() {
            let v = G[u][i];
            if v == p {
                continue;
            }
            queue.push_back((v, u));
        }
    }

    let S = calc(G, &W);

    (S, W)
}

fn main() {
    let N = read!(usize);
    let mut T = read![usize, usize; N - 1];
    let C = read![[i64]];

    for t in &mut T {
        t.0 -= 1;
        t.1 -= 1;
    }

    let mut G = vec![vec![]; N];
    for (u, v) in T {
        G[u].push(v);
        G[v].push(u);
    }

    let (s, c) = solve(&G, &C);
    debug_assert!({
        let (xs, xc) = brute_force(&G, &C);
        debug_assert_eq!(s, xs, "expected = {:?}", xc);
        xs == s
    });

    println!("{}", s);
    println!(
        "{}",
        c.into_iter()
            .map(|c| c.to_string())
            .collect::<Vec<_>>()
            .join(" ")
    );
}

/*

6
1 2
2 3
2 4
4 5
4 6
3 1 4 1 5 9

=>
14
3 4 1 5 1 9



7
1 2
1 5
2 3
2 4
5 6
5 7
3 1 4 1 5 9 2

=>
16
3 2 1 1 5 4 9

*/
