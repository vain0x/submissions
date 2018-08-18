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
    let (N, M, Q) = read!(usize, usize, usize);
    let mut es = vec![vec![0; N + 1]; N + 1];

    let stdout = std::io::stdout();
    let mut outl = stdout.lock();

    let stdin = std::io::stdin();
    let mut inl = stdin.lock();
    let mut line = String::new();

    for _ in 0..M {
        line.clear();
        inl.read_line(&mut line).unwrap();
        let mut tt = line.split_whitespace().map(|w| w.parse().unwrap());

        let l: usize = tt.next().unwrap();
        let r: usize = tt.next().unwrap();

        es[l][r] += 1;
    }

    for e in es.iter_mut() {
        for i in 0..N {
            e[i + 1] += e[i];
        }
    }

    // O(Q)
    for _ in 0..Q {
        line.clear();
        inl.read_line(&mut line).unwrap();
        let mut tt = line.split_whitespace().map(|w| w.parse().unwrap());

        let p: usize = tt.next().unwrap();
        let q: usize = tt.next().unwrap();

        let mut k = 0;

        for x in p..q + 1 {
            k += es[x][q];
        }

        writeln!(outl, "{}", k).unwrap()
    }
}
