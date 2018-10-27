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

// -----------------------------------------------
// Framework <https://github.com/vain0x/procon>
// -----------------------------------------------

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

fn trial(A: &Vec<i64>, b: bool) -> i64 {
    let mut A = A.to_owned();
    A.sort();

    let mut li = 0;
    let mut ri = A.len();
    let mut s;

    let (mut front, mut back, mut t) = if b {
        s = -2 * A[li];
        li += 1;
        (-A[li - 1], -A[li - 1], 0)
    } else {
        ri -= 1;
        s = 2 * A[ri];
        (A[ri], A[ri], 2)
    };

    debug!(A);
    while li < ri {
        debug!((t, li, ri, front, back, s));
        if t <= 1 {
            ri -= 1;

            s += 2 * A[ri];
            back = front;
            front = A[ri];
        } else {
            s -= 2 * A[li];
            back = front;
            front = -A[li];

            li += 1;
        }
        t = (t + 1) % 4;
    }

    s - front - back
}

fn main() {
    let N = read![usize];
    let A = read![i64; N];
    println!("{}", max(trial(&A, true), trial(&A, false)));
}
