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
    let N = read!(usize);
    let T = read!(i64, i64; N);

    let mut U = T
        .iter()
        .cloned()
        .enumerate()
        .map(|(i, (a, b))| (a + b, a, b, i))
        .collect::<Vec<_>>();
    U.sort();
    U.reverse();

    let mut V = T
        .iter()
        .cloned()
        .enumerate()
        .map(|(i, (a, b))| (a + b, b, a, i))
        .collect::<Vec<_>>();
    V.sort();
    V.reverse();

    let mut done = vec![false; N];
    let mut ti = 0;
    let mut ai = 0;
    let mut turn = true;
    let mut score = 0_i64;
    while ti < N || ai < N {
        if turn {
            // takahashi
            while ti < N {
                let (_, a, b, i) = U[ti];
                ti += 1;
                if done[i] {
                    continue;
                }
                score += a;
                done[i] = true;
                debug!(("takahashi", i, a, score));
                break;
            }
        } else {
            // aoki
            while ai < N {
                let (_, b, a, i) = V[ai];
                ai += 1;
                if done[i] {
                    continue;
                }
                score -= b;
                done[i] = true;
                debug!(("aoki", i, b, score));
                break;
            }
        }
        turn = !turn;
    }

    println!("{}", score)
}
