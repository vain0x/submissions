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

static W: &'static [usize] = &[0, 2, 5, 5, 4, 5, 6, 3, 7, 6];

fn dfs(i: usize, w: usize, A: &[usize], bag: &mut [usize]) -> bool {
    if i >= A.len() {
        return w == 0;
    }

    for k in (0..w / W[A[i]] + 1).rev() {
        bag[A[i]] = k;
        let ok = dfs(i + 1, w - k * W[A[i]], A, bag);
        if ok {
            return true;
        }
    }

    false
}

fn main() {
    let (N, _) = read!(usize, usize);
    let mut A = read![[usize]];

    A.sort();
    A.reverse();

    let mut min_d = 0;
    let mut min_w = 99;
    for &d in &A {
        if min_w > W[d] {
            min_w = W[d];
            min_d = d;
        }
    }
    debug!(N, A, (min_d, min_w));

    let d = min_d;
    let mut l = N / W[d];
    loop {
        let mut bag = vec![0; 10];

        let w = N - l * W[d];
        let ok = dfs(0, w, &A, &mut bag);
        debug!((l, w), bag);

        if !ok {
            l -= 1;
            continue;
        }

        bag[d] += l;

        let mut s = vec![];
        for d in (1..10).rev() {
            for _ in 0..bag[d] {
                s.push(b'0' + d as u8);
            }
        }
        println!("{}", String::from_utf8(s).unwrap());
        break;
    }
}
