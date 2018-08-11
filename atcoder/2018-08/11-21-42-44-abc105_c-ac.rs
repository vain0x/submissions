#![allow(unused_imports)]
#![allow(non_snake_case)]

use std::cell::RefCell;
use std::cmp::{max, min, Ordering};
use std::collections::*;
use std::fmt::{Debug, Formatter};
use std::io::*;
use std::mem::{replace, swap};
use std::ops::*;
use std::rc::Rc;

// -----------------------------------------------
// Framework
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
        {
            let entries = [$(&stringify!([$arg]:), &$arg as &Debug),*];
            stderr().write_fmt(format_args!("{:?}\n", entries)).unwrap();
        }
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

pub fn main() {
    let mut n = read!(i64);

    if n == 0 {
        println!("0");
        return;
    }

    let mut v = vec![];
    let mut i = 1;

    while n != 0 {
        if n % (1 << i) != 0 {
            v.push(1);
            n -= (1 << (i - 1)) * (if i % 2 == 1 { 1 } else { -1 });
        } else {
            v.push(0)
        }

        debug!(n);
        i += 1;
    }

    {
        let mut m = 0;
        let mut p = 1;
        for b in v.iter().cloned() {
            m += b * p;
            p *= -2;
        }
        debug!(m);
    }

    println!(
        "{}",
        v.into_iter().rev().map(|b| b.to_string()).vec().join("")
    );
}
