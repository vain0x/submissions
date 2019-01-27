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
    let (N, M) = read!(usize, usize);

    let mut G = vec![vec![]; N];
    // let mut H = vec![vec![]; N];
    let mut deg = vec![0; N];

    for i in 0..(N + M - 1) {
        let (A, B) = read!(usize, usize);
        G[A - 1].push((i, B - 1));
        // H[B - 1].push(i);
        deg[B - 1] += 1;
    }

    let mut root = 0;
    for i in 0..N {
        if deg[i] == 0 {
            root = i;
            break;
        }
    }
    debug!(root);

    let mut Q = VecDeque::new();
    Q.push_back(root);
    // let mut acc = vec![false; N + M - 1];
    // let mut rej = vec![false; N + M - 1];
    let mut par = vec![0; N];

    while let Some(q) = Q.pop_front() {
        // debug!(q);
        for i in 0..G[q].len() {
            let (_, r) = G[q][i];
            // debug!((q, e, r, deg[r]));
            if deg[r] != 1 {
                // rej[e] = true;
                deg[r] -= 1;
                continue;
            }
            // acc[e] = true;
            par[r] = 1 + q;
            Q.push_back(r);
        }
    }

    for i in 0..N {
        println!("{}", par[i]);
    }
}
