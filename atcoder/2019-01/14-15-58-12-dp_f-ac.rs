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

fn main() {
    let S = rl().chars().collect::<Vec<_>>();
    let T = rl().chars().collect::<Vec<_>>();
    let N = S.len();
    let M = T.len();

    let mut len = vec![vec![0; M + 1]; N + 1];
    let mut last = vec![vec![(0, 0); M + 1]; N + 1];

    for si in 0..N + 1 {
        for ti in 0..M + 1 {
            if si < N && len[si + 1][ti] < len[si][ti] {
                len[si + 1][ti] = len[si][ti];
                last[si + 1][ti] = last[si][ti];
            }

            if ti < M && len[si][ti + 1] < len[si][ti] {
                len[si][ti + 1] = len[si][ti];
                last[si][ti + 1] = last[si][ti];
            }

            if si < N && ti < M && S[si] == T[ti] {
                len[si + 1][ti + 1] = len[si][ti] + 1;
                last[si + 1][ti + 1] = (si, ti);
            }
        }
    }

    let mut U = vec![];
    {
        let mut si = N;
        let mut ti = M;

        while len[si][ti] > 0 {
            let (lsi, lti) = last[si][ti];
            U.push(S[lsi]);

            si = lsi;
            ti = lti;
        }
    }

    debug!((len, last));

    println!("{}", U.into_iter().rev().collect::<String>())
}
