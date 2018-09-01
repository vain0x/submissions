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
    const R: usize = 3;

    let L = read!(usize);

    let N;
    let mut es = vec![];

    if L < R {
        N = 2;
        for i in 0..L {
            es.push((1, 2, i));
        }
    } else {
        let mut h = 0;
        let mut p = 1;
        while p * R <= L {
            p *= R;
            h += 1;
        }
        N = h + 1;

        let mut q = p;
        for i in 1..N {
            q /= R;

            for k in 0..R {
                es.push((i, i + 1, k * q));
            }
        }

        // s 未満の経路は生成済み
        let mut s = p;

        // j に到達すると長さ p 以下の経路が列挙される
        let mut j = 1;

        // debug!((p, s, j));

        while s < L {
            let t = L - s;

            while j <= 1 || p > t {
                p /= R;
                h -= 1;
                j += 1;
            }

            // debug!((p, s, t, j));

            // t + p 未満の経路が生成される
            es.push((1, j, s));

            s += p;
        }
    }

    let M = es.len();

    if N > 20 {
        panic!("too many vertices");
    }

    if M > 60 {
        panic!("too many edges");
    }

    println!("{} {}", N, M);
    for (u, v, w) in es {
        println!("{} {} {}", u, v, w);
    }
}
