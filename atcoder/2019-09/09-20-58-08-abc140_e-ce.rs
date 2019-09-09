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
    let mut P = read![[usize]];

    // P の前後に番兵として N より大きい要素を2個ずつ追加する。
    P.insert(0, N + 1);
    P.insert(1, N + 2);
    P.push(N + 3);
    P.push(N + 4);

    // 番兵でない値の範囲は P[2..N+2]
    let min_i = 2;
    let max_i = N + 1;

    // 処理済み要素のインデックスの集合
    // BTreeSet は要素の重複を省くので、解説PDFにあるように 0 を2個入れることはできない。
    // 後で max 関数を使って補正する。
    let mut S = BTreeSet::new();
    S.insert(0);
    S.insert(1);
    S.insert(N + 2);
    S.insert(N + 3);

    // P の値からインデックスを逆算するもの
    let mut pos = vec![0; N + 6];
    for i in 0..P.len() {
        let p = P[i];
        pos[p] = i;
    }

    debug!(P);

    let mut sum = 0_usize;

    for p in (1..N + 1).rev() {
        let i = pos[p];

        // S に含まれる i より小さい要素を降順に列挙するイテレータ
        // 番兵のおかげで必ず2つ以上の要素を列挙できる。
        let mut prev_iter = S.range(..i).rev();
        let x = max(min_i - 1, *prev_iter.next().unwrap());
        let w = max(min_i - 1, *prev_iter.next().unwrap());
        assert!(w <= x && x < i);

        // i より大きい要素を昇順に列挙するイテレータ
        let mut next_iter = S.range(i + 1..);
        let y = min(max_i + 1, *next_iter.next().unwrap());
        let z = min(max_i + 1, *next_iter.next().unwrap());
        assert!(i < y && y <= z);

        // i より左側にある p より大きい要素をちょうど1個含むような区間の個数
        let left = (x - w) * (y - i);

        // 右側
        let right = (i - x) * (z - y);

        let count = left + right;

        sum += count * p;

        debug!(p, (w, x, i, y, z), (left, right, count, sum));

        // i を処理済みにする。
        S.insert(i);
    }

    println!("{}", sum)
}
