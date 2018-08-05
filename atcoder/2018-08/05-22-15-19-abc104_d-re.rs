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

pub fn recurse<X, Y>(x: X, f: &Fn(X, &Fn(X) -> Y) -> Y) -> Y {
    f(x, &|x: X| recurse(x, &f))
}

pub fn main() {
    let S = rl().chars().vec();
    let N = S.len();

    let P = 1_000_000_007_i64;
    let ABC = "ABC".chars().vec();

    let k = {
        let mut memo = vec![vec![None; N + 1]; 3 + 1];
        let memo = RefCell::new(memo);
        recurse((0, 0), &|(i, r), go| {
            if i == N {
                return if r == 3 { 1 } else { 0 };
            }

            if let Some(result) = memo.borrow()[r][i] {
                return result;
            }

            let result: i64 = if r < 3 && S[i] == ABC[r] {
                (go((i + 1, r)) + go((i + 1, r + 1))) % P
            } else if r < 3 && S[i] == '?' {
                ((go((i + 1, r)) * 3) % P + go((i + 1, r + 1))) % P
            } else if r == 3 && S[i] == '?' {
                go((i + 1, r)) * 3 % P
            } else {
                go((i + 1, r))
            };

            memo.borrow_mut()[r][i] = Some(result);
            result
        })
    };
    println!("{}", k);
}
