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

// 順列 perm の前から k 要素 (perm[..k]) は確定済みとする。
// 残りの部分 (perm[k..]) のすべての並び替えについて整数の書き方を試し、
// 条件を満たす書き方を数える。
fn dfs(k: usize, a: &[usize], n: usize, perm: &mut [usize], board: &mut Vec<Vec<usize>>) -> i64 {
    if k == n {
        // 整数を盤面に書き込む。
        let mut i = 0;
        for x in 0..3 {
            for y in 0..a[x] {
                board[x][y] = perm[i];
                i += 1;
            }
        }

        // 条件を確認する。
        let mut ok = true;
        for x in 0..3 {
            for y in 0..a[x] {
                if y >= 1 && board[x][y] <= board[x][y - 1] {
                    ok = false;
                }

                if x >= 1 && board[x][y] <= board[x - 1][y] {
                    ok = false;
                }
            }
        }

        // 条件を満たさないならカウントしない。
        if !ok {
            return 0;
        }

        return 1;
    }

    let mut total = 0;

    for s in k..n {
        // k 番目と s 番目を交換する。
        let pk = perm[k];
        let ps = perm[s];
        perm[k] = ps;
        perm[s] = pk;

        total += dfs(k + 1, a, n, perm, board);

        // 上記の変更をキャンセルする。
        perm[k] = pk;
        perm[s] = ps;
    }

    total
}

fn main() {
    let a = read![[usize]];

    let n = a.iter().sum::<usize>();
    let mut perm = (0..n).collect::<Vec<_>>();
    let mut board = vec![vec![0; 3]; 3];
    let count = dfs(0, &a, n, &mut perm, &mut board);

    println!("{}", count);
}
