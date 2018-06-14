#![allow(unused_imports)]
#![allow(non_snake_case)]

use std::cmp::{max, min, Ordering};
use std::collections::*;
use std::io::*;
use std::ops::*;
use std::*;

// -----------------------------------------------
// Framework
// -----------------------------------------------

#[allow(unused)]
fn rl() -> String {
    let mut buf = String::new();
    io::stdin().read_line(&mut buf).unwrap();
    buf.trim_right().to_owned()
}

#[allow(unused_macros)]
macro_rules! read {
    ([$t:ty] ; $n:expr) => {
        (0..$n).map(|_| read!([$t])).collect::<Vec<_>>()
    };
    ($($t:ty),+ ; $n:expr) => {
        (0..$n).map(|_| read!($($t),+)).collect::<Vec<_>>()
    };
    ([$t:ty]) => {{
        rl()
            .split_whitespace()
            .map(|word| word.parse().unwrap())
            .collect::<Vec<$t>>()
    }};
    ($t:ty) => {
        rl().parse::<$t>().unwrap()
    };
    ($($t:ty),*) => {{
        let buf = rl();
        let mut w = buf.split_whitespace().into_iter();
        ($(w.next().unwrap().parse::<$t>().unwrap()),*)
    }};
}

// -----------------------------------------------
// Solution
// -----------------------------------------------

pub fn recurse<X, Y, F: FnMut(&mut FnMut(X) -> Y, X) -> Y>(x: X, mut f: F) -> Y {
    fn call<X, Y>(x: X, f: &mut FnMut(&mut FnMut(X) -> Y, X) -> Y) -> Y {
        let fp = f as *mut FnMut(&mut FnMut(X) -> Y, X) -> Y;
        let f1 = unsafe { &mut *fp };
        let f2 = unsafe { &mut *fp };
        f1(&mut |x: X| call(x, f2), x)
    }
    call(x, &mut f)
}

pub fn main() {
    let (N, M) = read!(usize, usize);
    let mut g = vec![vec![]; N + M];
    for v in 0..N {
        let w = read!([usize]);
        for &w in &w[1..] {
            g[v].push(N + w - 1);
            g[N + w - 1].push(v);
        }
    }

    let mut done = vec![false; N + M];
    recurse(0, |go, v| {
        if done[v] {
            return;
        }
        done[v] = true;
        for &w in g[v].iter() {
            go(w);
        }
    });

    println!("{}", if (0..N).all(|v| done[v]) { "YES" } else { "NO" });
    return;
}
