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

#[cfg(debug_assertions)]
include!{"./procon/debug.rs"}

#[cfg(not(debug_assertions))]
macro_rules! debug {
    ($($arg:expr),*) => {};
}

#[allow(unused_macros)]
macro_rules! read {
    ([$t:ty] ; $n:expr) =>
        ((0..$n).map(|_| read!([$t])).collect::<Vec<_>>());
    ($($t:ty),+ ; $n:expr) =>
        ((0..$n).map(|_| read!($($t),+)).collect::<Vec<_>>());
    ([$t:ty]) =>
        (rl().split_whitespace().map(|w| w.parse().unwrap()).collect::<Vec<$t>>());
    ($t:ty) =>
        (rl().parse::<$t>().unwrap());
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

trait IteratorExt: Iterator + Sized {
    fn vec(self) -> Vec<Self::Item> {
        self.collect()
    }
}

impl<T: Iterator> IteratorExt for T {}

// -----------------------------------------------
// Solution
// -----------------------------------------------

fn main() {
    let N = read!(usize);
    let B = read![i64, i64; N];

    let mut p = (B[0].0 + B[0].1).abs() % 2;
    for &(x, y) in B.iter() {
        if p != (x + y).abs() % 2 {
            println!("-1");
            return;
        }

        p = (x + y).abs() % 2;
    }

    let m = 40 - p as usize;
    let mut ds = vec![1; m];

    // Pairs of 4^i .
    for i in 0..15 {
        ds[2 * i] = 1 << (2 * i);
        ds[2 * i + 1] = 1 << (2 * i);
    }

    ds.sort();
    ds.reverse();

    let mut wss = vec![];

    for (mut x, mut y) in B {
        let mut ws = vec![];

        for wi in 0..m as usize {
            let d = ds[wi];

            let mi = min(
                min((x + d).abs(), (x - d).abs()) + y.abs(),
                x.abs() + min((y + d).abs(), (y - d).abs()),
            );

            if mi == (x - d).abs() + y.abs() {
                ws.push('R');
                x -= d;
            } else if mi == (x + d).abs() + y.abs() {
                ws.push('L');
                x += d;
            } else if mi == x.abs() + (y - d).abs() {
                ws.push('U');
                y -= d;
            } else {
                ws.push('D');
                y += d;
            }
        }

        let ws = ws.into_iter().collect::<String>();
        assert_eq!((x, y), (0, 0), "{}", ws);
        wss.push(ws);
    }

    println!("{}", m);
    println!("{}", ds.iter().map(|d| d.to_string()).vec().join(" "));
    for ws in wss {
        println!("{}", ws);
    }
}
