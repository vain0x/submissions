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
    let (_, K) = read!(usize, usize);
    let S = rl().chars().collect::<Vec<_>>();

    let mut max_sum = 0;

    // 注目する区間の左端
    let mut l = 0;

    // 注目する区間の右端
    let mut r = 0;

    // 注目している区間に含まれる、0 が連続する部分の個数
    // 例えば 010001 なら zero_count = 2
    let mut zero_count = 0;

    while l < S.len() {
        while r < S.len() {
            // 次に c が連続する区間を最長区間に取り込む。
            let c = S[r];

            // 次が 0 からなる区間なら反転操作の回数が増える。
            if c == '0' {
                if zero_count < K {
                    zero_count += 1;
                } else {
                    // 操作回数が上限に達しているので、区間をこれ以上伸ばすことはできない。
                    break;
                }
            }

            while r < S.len() && S[r] == c {
                r += 1;
            }
        }

        max_sum = max(max_sum, r - l);

        // 注目している区間の左端にある、0 または 1 が連続している部分を捨てる。
        let c = S[l];
        if c == '0' {
            zero_count -= 1;
        }
        while l < S.len() && S[l] == c {
            l += 1;
        }
    }

    println!("{}", max_sum)
}
