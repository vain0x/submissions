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

#[allow(unused_macros)]
macro_rules! debug {
    ($($arg:expr),*) => {
        #[cfg(debug_assertions)]
        $(writeln!(stderr(), "{} = {:?}", stringify!($arg), $arg).unwrap());*
    };
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
    let (H, W) = read!(usize, usize);
    let A = read![[i32]; H];

    let mut ops = vec![];
    let mut prev = None;

    for y in 0..H {
        for x in 0..W {
            let x = if y % 2 == 0 { x } else { W - 1 - x };
            let mut a = A[y][x];

            if let Some((y1, x1)) = prev {
                a += 1;
                ops.push((y1, x1, y, x));
                prev = None;
            }

            if a % 2 != 0 {
                prev = Some((y, x));
            }
        }
    }

    println!("{}", ops.len());
    for (y1, x1, y2, x2) in ops {
        println!("{} {} {} {}", 1 + y1, 1 + x1, 1 + y2, 1 + x2)
    }
}
