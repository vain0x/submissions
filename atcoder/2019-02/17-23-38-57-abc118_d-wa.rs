// -----------------------------------------------
// Framework <https://github.com/vain0x/procon>
//
// See the bottom of file for solution.
// -----------------------------------------------

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
    buf.trim_right().to_owned()
}

// -----------------------------------------------
// Solution
// -----------------------------------------------

static W: &'static [usize] = &[0, 2, 5, 5, 4, 5, 6, 3, 7, 6];

fn main() {
    let (N, _) = read!(usize, usize);
    let A = read![[usize]];

    // dp[n] = l : マッチの残り本数が w　本になったとき、作った数字の個数の最大値が l
    let mut dp = vec![0; N + 1];

    // prev[n] = d : dp[n] の値を更新したときに作った数字が d
    let mut prev = vec![0; N + 1];

    for n in (0..N + 1).rev() {
        for &d in &A {
            let w = W[d];
            if n < w {
                continue;
            }

            if dp[n - w] < dp[n] + 1 {
                dp[n - w] = dp[n] + 1;

                // dp[n] の状態から、数字 d を作ることによって dp[n - w] の最適な状態に遷移できた、ということを記録しておく。
                prev[n - w] = d;
            }
        }
    }

    // 作った数字のリスト
    let mut digits = vec![];

    // dp[0] に至る経路を逆順に辿ることで、実際に桁数が dp[0] になるようなマッチの使いかた (作る数字のリスト) を再構成する。(経路復元)
    let mut w = 0;
    while w < N {
        let d = prev[w];
        digits.push(b'0' + d as u8);
        w += W[d];
    }

    // 数字を大きい順に並べ替えることで整数を最大化する。
    digits.sort();
    digits.reverse();

    // 数字のリストを文字列にする。
    let answer = String::from_utf8(digits).unwrap();

    println!("{}", answer);
}
