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

fn main() {
    // let N = 2_000_i64;
    // let M = 2_000_i64;
    // let mut S = (0..N)
    //     .map(|i| (1 + i * i % 99_993 * i % 99_993).to_string())
    //     .collect::<Vec<_>>()
    //     .join(" ");
    // let mut T = (0..M)
    //     .map(|i| (1 + i * i % 99_993 * i % 99_997).to_string())
    //     .collect::<Vec<_>>()
    //     .join(" ");

    // println!("{} {}", N, M);
    // println!("{}", S);
    // println!("{}", T);

    let (sn, tn) = read!(usize, usize);
    let S = read![[i64]];
    let T = read![[i64]];

    let mut inc = vec![0; sn + 1];

    // dp[si] = n : S[..si] の部分列と T[..ti] の部分列の組み合わせの総数
    let mut dp = vec![1; sn + 1];

    for ti in 0..tn {
        for z in 0..sn {
            if S[z] == T[ti] {
                inc[z] += dp[z];
                inc[z] %= P;
            }
        }

        for si in 0..sn {
            dp[si + 1] = dp[si] + inc[si] % P;
            dp[si + 1] %= P;
        }

        // debug!(ti, inc, dp);
    }

    println!("{}", dp[sn])
}

/*

4 3
1 2 3 1
2 1 3

#=> 10

*/
