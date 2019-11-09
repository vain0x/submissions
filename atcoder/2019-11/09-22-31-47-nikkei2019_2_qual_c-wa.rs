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

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Rev<T>(pub T);

impl<T: PartialOrd> PartialOrd for Rev<T> {
    fn partial_cmp(&self, other: &Rev<T>) -> Option<Ordering> {
        other.0.partial_cmp(&self.0)
    }
}

impl<T: Ord> Ord for Rev<T> {
    fn cmp(&self, other: &Rev<T>) -> Ordering {
        other.0.cmp(&self.0)
    }
}

fn solve(A: &[i64], B: &[i64]) -> bool {
    let N = A.len();

    let mut count = 0;

    // A の置換
    let mut p = (0..N).collect::<Vec<_>>();

    // 未修復の位置が A の値が大きい順に出てくるキュー
    let mut xq = (0..N).map(|i| (A[i], i, i)).collect::<BinaryHeap<_>>();

    // B >= api である位置が A の値が小さい順に出てくるキュー
    let mut aq = BinaryHeap::new();

    // B < api である位置が B の値が大きい順に出てくるキュー
    let mut bq = (0..N).map(|i| (B[i], i)).collect::<BinaryHeap<_>>();

    while let Some((api, i, pi)) = xq.pop() {
        if p[i] != pi {
            // 情報が古い
            continue;
        }
        debug_assert_eq!(api, A[p[i]]);

        if A[p[i]] <= B[i] {
            // 修復不要
            continue;
        }

        debug!((api, i, pi));

        // api の変化に合わせてキューの不変条件を復元する
        // 破綻修復のための交換先の候補を求めることに相当
        while let Some(&(b, j)) = bq.peek() {
            debug_assert_eq!(b, B[j]);

            if b >= api {
                bq.pop();
                aq.push((Rev(A[p[j]]), j, p[j]));
            } else {
                break;
            }
        }

        while let Some(&(Rev(apj), j, pj)) = aq.peek() {
            if p[j] != pj {
                // 情報が古い
                aq.pop();
                continue;
            }
            debug_assert_eq!(apj, A[p[j]]);
            break;
        }

        // 最良の交換先
        let (Rev(apj), j, _) = match aq.pop() {
            None => return false,
            Some(x) => x,
        };
        debug_assert_eq!(apj, A[p[j]]);
        debug_assert!(api <= B[j]);

        if apj > B[i] {
            return false;
        }
        debug_assert!(apj <= B[i]);

        // 交換
        debug!(("swap", i, j));
        p.swap(i, j);
        count += 1;

        debug_assert!(A[p[i]] <= B[i]);
        debug_assert!(A[p[j]] <= B[j]);

        // キューに交換後の情報を詰めなおす
        for &i in &[i, j] {
            xq.push((A[p[i]], i, p[i]));

            if B[i] >= api {
                aq.push((Rev(A[p[i]]), i, p[i]));
            }
        }

        if count > N - 2 {
            return false;
        }
    }

    true
}

fn main() {
    read!(usize);
    let A = read![[i64]];
    let B = read![[i64]];
    println!("{}", if solve(&A, &B) { "Yes" } else { "No" });
}
