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

// 参考: https://atcoder.jp/contests/abc124/submissions/4962823

fn main() {
    let (N, K) = read!(usize, usize);
    let S = rl().chars().collect::<Vec<_>>();

    let mut nums = vec![];
    let mut now = '1';
    let mut cnt = 0;
    for i in 0..N {
        if S[i] == now {
            cnt += 1;
        } else {
            nums.push(cnt);
            now = if now == '0' { '1' } else { '0' };
            cnt = 1;
        }
    }
    if cnt != 0 {
        nums.push(cnt);
    }
    if nums.len() % 2 == 0 {
        nums.push(0);
    }

    let add = 2 * K + 1;
    let mut answer = 0;

    let mut i = 0;
    while i < nums.len() {
        let mut sum = 0;
        let r = min(i + add, nums.len());
        for j in i..r {
            sum += nums[j];
        }
        answer = max(answer, sum);
        i += 2;
    }

    println!("{}", answer)
}
