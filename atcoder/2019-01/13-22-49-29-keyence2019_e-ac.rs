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

fn wei(i: usize, j: usize) -> i64 {
    (i as i64 - j as i64).abs()
}

fn main() {
    let (N, D) = read!(usize, i64);
    let A = read![[i64]];

    let mut L = vec![0; N];
    let mut R = vec![0; N];

    let mut li = 0;
    for i in 1..N {
        let l = wei(i, li) * D + A[li];
        if l < A[i] {
            L[i] = li;
        } else {
            L[i] = i;
            li = i;
        }
    }

    let mut ri = N - 1;
    R[ri] = ri;
    for i in (0..N - 1).rev() {
        let r = wei(i, ri) * D + A[ri];
        if r < A[i] {
            R[i] = ri;
        } else {
            R[i] = i;
            ri = i;
        }
    }

    let mut roots = vec![];
    let mut W = 0_i64;
    for i in 0..N {
        let li = L[i];
        let ri = R[i];
        if li == i && ri == i {
            roots.push(i);
            continue;
        }

        let l = if i == li {
            std::i64::MAX
        } else {
            wei(i, li) * D + A[li]
        };

        let r = if i == ri {
            std::i64::MAX
        } else {
            wei(i, ri) * D + A[ri]
        };

        debug!((i, li, ri, l, r));
        W += min(l, r) + A[i];
    }
    debug!(roots);

    for win in roots.windows(2) {
        let i = win[0];
        let j = win[1];
        let w = wei(i, j) * D + A[i] + A[j];
        W += w;
        debug!((i, j, w));
    }

    println!("{}", W);
}
