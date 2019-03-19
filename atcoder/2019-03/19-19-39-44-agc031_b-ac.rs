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

fn solve() {
    let N = read!(usize);
    let mut C = read![usize; N];

    for i in 0..N {
        C[i] -= 1;
    }
    let M = C.iter().cloned().max().unwrap();

    let mut last = vec![N; M + 1];
    let mut next = vec![N + 1; N + 1];
    let mut i = 0;
    while i < N {
        while i + 1 < N && C[i] == C[i + 1] {
            i += 1;
        }

        next[last[C[i]]] = i;
        last[C[i]] = i;
        i += 1
    }
    debug!(next);

    // dp[r]: 0..r の塗り方が決まって、r.. に操作が及んでないときの場合の数

    let mut dp = vec![P; N + 2];
    dp[N] = 1;
    dp[N + 1] = 0; // 位置 i より後に同色の石がないとき dp[next[i]] == 0 にしておけば next[i] が存在するかどうかの場合分けを潰せる

    let mut i = N - 1;
    loop {
        dp[i] = dp[i + 1];
        dp[i] += dp[next[i]];
        dp[i] %= P;

        if i == 0 {
            break;
        }
        i -= 1;
    }
    debug!(dp);

    println!("{}", dp[0]);
}

fn main() {
    solve();
}
