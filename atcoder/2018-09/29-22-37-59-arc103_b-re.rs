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

    let mut p = 0;
    let mut ok = true;

    for i in 0..N {
        let (x, y) = B[i];
        let q = (x + y).abs() % 2;
        if i == 0 {
            p = q;
        } else {
            if p != q {
                ok = false;
                break;
            }
        }

        if !(x.abs() + y.abs() <= 100) {
            panic!("give me partial points")
        }
    }

    if !ok {
        println!("-1");
        return;
    }

    let m = 40 - p as usize;
    let mut ds = vec![];
    let mut wss = vec![];

    for wi in 0..m {
        let d = if wi < 20 { 5 } else { 1 };
        ds.push(d);
    }

    for i in 0..N {
        let (mut x, mut y) = B[i];

        let mut ws = vec![];
        for wi in 0..m {
            let d = ds[wi];

            if x.abs() > y.abs() {
                if x > 0 {
                    ws.push('R');
                    x -= d;
                } else {
                    ws.push('L');
                    x += d;
                }
            } else {
                if y > 0 {
                    ws.push('U');
                    y -= d;
                } else {
                    ws.push('D');
                    y += d;
                }
            }
        }

        debug_assert_eq!((x, y), (0, 0));
        wss.push(ws.into_iter().collect::<String>());
    }

    println!("{}", m);
    println!("{}", ds.iter().map(|&x| x.to_string()).vec().join(" "));
    for ws in wss {
        println!("{}", ws);
    }
}
