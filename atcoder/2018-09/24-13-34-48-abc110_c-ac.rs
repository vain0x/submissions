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
    // 解説どおり

    let S = rl().chars().vec();
    let T = rl().chars().vec();

    let mut f = BTreeMap::new();
    let mut g = BTreeMap::new();

    let mut ok = true;

    for i in 0..S.len() {
        let (s, t) = (S[i], T[i]);

        if let Some(&s) = f.get(&s) {
            debug!((i, S[i], T[i], s, t));
            ok = ok && s == t;
        } else if let Some(&t) = g.get(&t) {
            debug!((i, S[i], T[i], s, t));
            ok = ok && s == t;
        } else {
            debug!((i, s, t));
            f.insert(s, t);
            g.insert(t, s);
        }
    }

    println!("{}", if ok { "Yes" } else { "No" })
}
