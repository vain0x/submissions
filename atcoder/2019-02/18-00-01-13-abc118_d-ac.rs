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

// W[d] = w : 数字 d を作るのに必要なマッチの本数が w
static W: &'static [usize] = &[0, 2, 5, 5, 4, 5, 6, 3, 7, 6]; // '

fn main() {
    let (N, _) = read!(usize, usize);
    let mut A = read![[usize]];

    // A を降順にする。
    A.sort();
    A.reverse();

    // dp[n] = l : マッチをちょうど n 本使ったとき、作った数字の個数の最大値が l
    let mut dp = vec![None; N + 1];
    dp[0] = Some(0);

    for n in 0..N + 1 {
        for &d in &A {
            let w = W[d];
            if n + w > N {
                continue;
            }

            let l = match dp[n] {
                None => continue,
                Some(l) => l,
            };

            dp[n + w] = match dp[n + w] {
                None => Some(l + 1),
                Some(max_l) => Some(max(max_l, l + 1)),
            };
        }
    }

    // 貪欲に dp[N] 桁の整数を構成する。
    let mut digits = vec![];
    let mut n = N;
    while n > 0 {
        let mut ok = false;

        for &d in &A {
            let w = W[d];
            if n < w {
                continue;
            }

            let l = dp[n].unwrap();

            // 数字 d を作った後に残るマッチ棒 (n - w) 本で残りの (l - 1) 桁を作れるなら、数字 d を作るのが最善。
            if dp[n - w] == Some(l - 1) {
                digits.push(b'0' + d as u8);
                n -= w;
                ok = true;
                break;
            }
        }

        assert!(ok);
    }

    // 数字のリストを文字列にする。
    let answer = String::from_utf8(digits).unwrap();

    println!("{}", answer);
}
