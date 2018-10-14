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
    let G = read![i64, i64; N];

    let all_a = G.iter().map(|&(a, _)| a).sum::<i64>();
    let all_b = G.iter().map(|&(_, b)| b).sum::<i64>();
    let mut mi = min(all_a, all_b);

    debug!(all_a, all_b);

    let mut ws = vec![];
    for (i, &(a, b)) in G.iter().enumerate() {
        ws.push((a, i));
        ws.push((b, i));
    }
    ws.sort();

    debug!(ws);

    // Whether if both A[v] and B[v] are in ws[..N] for some v.
    let mut ok = false;
    let mut vs = BTreeSet::new();
    for i in ws[0..N].iter().map(|&(_, i)| i) {
        ok = ok || vs.contains(&i);
        vs.insert(i);
    }

    let s = ws[0..N].into_iter().map(|&(w, _)| w).sum::<i64>();
    debug!(ok, s);

    if ok {
        mi = min(mi, s);
    } else {
        for w in 0..N {
            let mut s = s - min(G[w].0, G[w].1);
            let last = if w == ws[N].1 { ws[N + 1].0 } else { ws[N].0 };
            debug!((w, s, last));
            mi = min(mi, s + last);
        }
    }

    println!("{}", mi)
}
