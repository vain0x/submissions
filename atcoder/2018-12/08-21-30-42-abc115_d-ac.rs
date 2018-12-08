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
use std::fmt::{Debug, Formatter, Write as FmtWrite};
use std::io::{stderr, stdin, BufRead, Write};
use std::mem::{replace, swap};
use std::ops::*;
use std::rc::Rc;

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

#[allow(dead_code)]
fn rl() -> String {
    let mut buf = String::new();
    stdin().read_line(&mut buf).unwrap();
    buf.trim_right().to_owned()
}

// -----------------------------------------------
// Solution
// -----------------------------------------------

// レベル l バーガーの下から x 層を食べる
fn dfs(mut x: i64, l: usize, p: &mut i64, xs: &[i64], ps: &[i64]) {
    // パン
    if x == 0 {
        return;
    }
    x -= 1;

    // l-1 バーガー
    if x < xs[l - 1] {
        return dfs(x, l - 1, p, xs, ps);
    }
    x -= xs[l - 1];
    *p += ps[l - 1];

    // パティ
    if x == 0 {
        return;
    }
    x -= 1;
    *p += 1;

    // l-1 バーガー
    if x == 0 {
        return;
    }
    if x < xs[l - 1] {
        return dfs(x, l - 1, p, xs, ps);
    }
    x -= xs[l - 1];
    *p += ps[l - 1];

    // パン
    if x == 0 {
        return;
    }
    x -= 1;
}

fn main() {
    let (N, X) = read!(usize, i64);

    let mut ps = vec![0; N + 1];
    let mut xs = vec![0; N + 1];
    ps[0] = 1;
    xs[0] = 1;

    for l in 0..N {
        ps[1 + l] = ps[l] * 2 + 1;
        xs[1 + l] = xs[l] * 2 + 3;
    }

    let mut p = 0_i64;
    dfs(X, N, &mut p, &xs, &ps);

    println!("{}", p)
}
