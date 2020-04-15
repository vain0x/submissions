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
    let mut a = read![[i64]];

    // 操作列
    let mut ops = vec![];

    // 絶対値が最大の要素を探す。
    let z = (0..N).max_by_key(|&i| a[i].abs()).unwrap();

    // 最大の絶対値を他の要素に加算することで、「すべて0以上」または「すべて0以下」にする。
    for i in 0..N {
        if i != z {
            ops.push((z, i));
            a[i] += a[z];
        }
    }

    // 累積和を取る。
    if a[0] >= 0 {
        for i in 0..N - 1 {
            ops.push((i, i + 1));
            a[i + 1] += a[i];
            debug_assert!(a[i] <= a[i + 1]);
        }
    } else {
        for i in (1..N).rev() {
            ops.push((i, i - 1));
            a[i - 1] += a[i];
            debug_assert!(a[i - 1] <= a[i]);
        }
    }

    println!("{}", ops.len());
    for (x, y) in ops {
        println!("{} {}", x + 1, y + 1);
    }
}
