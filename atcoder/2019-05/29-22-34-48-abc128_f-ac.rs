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

fn main() {
    let N = read!(usize);
    let S = read![[i64]];

    // A=N-1, B=1 で必ずスコア 0 を達成できる
    let mut max_sum = 0;

    // C = A - B
    for C in 1..N {
        // 踏んだ蓮のスコアの総和
        let mut sum = 0;

        // 踏んだ蓮の集合
        let mut done = BTreeSet::new();

        // ジャンプ回数 2x + 1 の経路を探索 (O(log N))
        let mut x = 0;
        loop {
            let A = N - 1 - min(N - 1, x * C);
            let B = A - min(A, C);
            if A == 0 || B == 0 {
                break;
            }

            // 最後のジャンプの1回前の戻りジャンプ
            let u = N - 1 - A;
            if !done.insert(u) {
                // 同じ蓮を踏んだ
                break;
            }
            sum += S[u];

            // 最初のジャンプ
            let v = A;
            if !done.insert(v) {
                break;
            }
            sum += S[v];

            max_sum = max(max_sum, sum);

            // 今のループで踏んだ蓮は次のループでも踏むので sum や done はそのままでよい
            x += 1;
        }
    }

    println!("{}", max_sum)
}
