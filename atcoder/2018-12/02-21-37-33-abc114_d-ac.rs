// -----------------------------------------------
// Framework <https://github.com/vain0x/procon>
//
// See the bottom of file for solution.
// -----------------------------------------------

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

#[allow(unused_macros)]
macro_rules! debug {
    ($($e:expr),*) => {
        #[cfg(debug_assertions)]
        $({
            let (e, mut err) = (stringify!($e), stderr());
            writeln!(err, "\x1B[33m{}\x1B[0m = {:?}", e, $e).unwrap()
        })*
    };
}

#[allow(unused_macros)]
macro_rules! read {
    ([$t:ty] ; $n:expr) =>
        ((0..$n).map(|_| read!([$t])).collect::<Vec<_>>());
    ($($t:ty),+ ; $n:expr) =>
        ((0..$n).map(|_| read!($($t),+)).collect::<Vec<_>>());
    ([$t:ty]) =>
        (rl().split_whitespace().map(|w| w.parse().unwrap()).collect::<Vec<$t>>());
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

// -----------------------------------------------
// Solution
// -----------------------------------------------

/// Performs prime factorization in O(√x) time.
pub fn factorize(mut x: i64) -> BTreeMap<i64, i64> {
    let mut ms = BTreeMap::new();
    let r = (x as f64).sqrt() as i64 + 1;

    for p in 2..r as i64 {
        let mut m = 0;

        while x >= p && x % p == 0 {
            x /= p;
            m += 1;
        }

        if m > 0 {
            ms.insert(p, m);
        }
    }

    // `x` can have a prime factor larger than √x at most one.
    if x > 1 {
        ms.insert(x, 1);
    }

    ms
}

fn main() {
    let N = read!(i64);

    let mut v = BTreeMap::new();
    for n in 2..N + 1 {
        let u = factorize(n);
        for (p, m) in u.into_iter() {
            *v.entry(p).or_insert(0) += m;
        }
    }
    debug!(v);

    let z = v.into_iter().map(|(_, m)| m).collect::<Vec<_>>();

    let mut m74 = 0;
    let mut m24 = 0;
    let mut m14 = 0;
    let mut m4 = 0;
    let mut m2 = 0;
    for &m in &z {
        if m >= 74 {
            m74 += 1;
        }
        if m >= 24 {
            m24 += 1;
        }
        if m >= 14 {
            m14 += 1;
        }
        if m >= 4 {
            m4 += 1;
        }
        if m >= 2 {
            m2 += 1;
        }
    }
    debug!(m2, m4);

    let mut s = 0_i64;

    // 2,4,4
    {
        let c2 = m2 - m4;

        // <4 を使うケース
        s += m4 * (m4 - 1) / 2 * c2;

        // <4 を使わないケース、 >=4 の3つ組のうちどれかを2にする
        s += 3 * m4 * (m4 - 1) * (m4 - 2) / 6;
    }

    // 4,14
    {
        let c4 = m4 - m14;

        // <14 を使うケース
        s += c4 * m14;

        // <14 を使わないケース
        s += m14 * (m14 - 1) / 2 * 2;
    }

    // 2,24
    {
        let c2 = m2 - m24;

        // <2 を使うケース
        s += c2 * m24;

        s += m24 * (m24 - 1) / 2 * 2;
    }

    // 74
    {
        s += m74;
    }

    println!("{}", s)
}
