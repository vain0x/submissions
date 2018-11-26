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

pub fn gcd_ext(x: i64, y: i64) -> (i64, i64, i64) {
    if y == 0 {
        (x.abs(), x.signum(), 0)
    } else {
        let (d, s, t) = gcd_ext(y, x % y);
        (d, t, s - x / y * t)
    }
}

fn chinese_reminder(a1: i64, n1: i64, a2: i64, n2: i64) -> Option<(i64, i64)> {
    let (d, x, _) = gcd_ext(n1, n2);
    if (a2 - a1).abs() % d != 0 {
        return None;
    }

    let m = n1 * (n2 / d);
    let t = (a2 - a1) / d * x % (n2 / d);
    let u = ((a1 + n1 * t) % m + m) % m;
    Some((u, m))
}

fn main() {
    let A = read![i64; 29];
    let A = [0, 0]
        .into_iter()
        .cloned()
        .chain(A.into_iter())
        .collect::<Vec<_>>();

    let mut u = Some((0, 1));
    for p in 1..30 {
        if let Some((x, n)) = u {
            u = chinese_reminder(x, n, A[p + 1], p as i64);
        }
    }
    debug!(u);

    let result;
    if let Some((x, _)) = u {
        let mut valid = x <= 1_000_000_000_000_i64;
        for i in 2..30_usize {
            let mut s = 0;
            let p = i as i64;
            let mut n = x;
            while n > 0 {
                s += n % p;
                n /= p;
            }
            debug!((i, s, A[i]));

            valid = valid && s as i64 == A[i];
        }
        result = if valid { Some(x) } else { None };
    } else {
        result = None;
    }

    if let Some(x) = result {
        println!("{}", x);
    } else {
        println!("invalid");
    }
}
