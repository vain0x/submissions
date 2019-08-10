//! ----------------------------------------------
//! Framework <https://github.com/vain0x/procon>
//!
//! See the bottom of file for solution.
//! ----------------------------------------------

#![allow(unused_imports)]
#![allow(non_snake_case)]

use std::cell::RefCell;
use std::cmp::{max, min, Ordering};
use std::collections::*;
use std::fmt::{Debug, Display, Formatter, Write as FmtWrite};
use std::io::{stderr, stdin, BufRead, Write};
use std::mem::{replace, swap};
use std::ops::*;
use std::rc::Rc;

/// Print values to standard error if debug mode.
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

/// Read from standard input and parse each word.
/// - `read!(T, U, ..)` parses a line as a tuple of words.
/// - `read![[T]]` parses a line as an array of words.
/// - `read![..; N]` parses `N` lines, using `read!(..)` repeatedly.
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

/// Read a line from standard input.
#[allow(dead_code)]
fn rl() -> String {
    let mut buf = String::new();
    stdin().read_line(&mut buf).unwrap();

    #[allow(deprecated)]
    buf.trim_right().to_owned()
}

// -----------------------------------------------
// Solution
// -----------------------------------------------

pub fn pow(x: i64, n: i64, p: i64) -> i64 {
    let (mut x, mut y, mut n) = (x % p, 1_i64, n);
    while n > 0 {
        if n % 2 != 0 {
            y = (y * x) % p;
            n -= 1;
        }

        x = (x * x) % p;
        n /= 2;
    }
    y
}

#[derive(Clone, Debug)]
enum X {
    Scalar(i64),
    Var,
    Add(Rc<X>, Rc<X>),
    Sub(Rc<X>, Rc<X>),
    Mul(Rc<X>, Rc<X>),
}

impl X {
    fn calc(&self, x: i64) -> i64 {
        match self {
            &X::Scalar(c) => c,
            &X::Var => x,
            &X::Add(ref l, ref r) => l.calc(x) + r.calc(x),
            &X::Sub(ref l, ref r) => l.calc(x) - r.calc(x),
            &X::Mul(ref l, ref r) => l.calc(x) * r.calc(x),
        }
    }

    fn expand(&self, p: i64) -> Vec<i64> {
        match self {
            &X::Scalar(c) => vec![c],
            &X::Var => vec![0, 1],
            &X::Add(ref l, ref r) => {
                let mut l = l.expand(p);
                let r = r.expand(p);
                while l.len() < r.len() {
                    l.push(0);
                }
                for i in 0..min(l.len(), r.len()) {
                    l[i] += r[i];
                    l[i] %= p;
                }
                while l.last().cloned() == Some(0) {
                    l.pop();
                }
                l
            }
            &X::Sub(ref l, ref r) => {
                let mut l = l.expand(p);
                let r = r.expand(p);
                while l.len() < r.len() {
                    l.push(0);
                }
                for i in 0..min(l.len(), r.len()) {
                    l[i] -= r[i];
                    l[i] %= p;
                }
                while l.last().cloned() == Some(0) {
                    l.pop();
                }
                l
            }
            &X::Mul(ref l, ref r) => {
                let l = l.expand(p);
                let r = r.expand(p);
                debug!(l, r);
                let mut z = vec![0; l.len() + r.len()];
                for i in 0..l.len() {
                    for j in 0..r.len() {
                        z[i + j] += l[i] * r[j] % p;
                        z[i + j] %= p;
                    }
                }
                while z.last().cloned() == Some(0) {
                    z.pop();
                }
                debug!(z);
                z
            }
        }
    }
}

fn main() {
    let p = read!(i64);
    let a = read![[i64]];

    // if a.iter().all(|&r| r == 0) {
    //     println!(
    //         "{}",
    //         std::iter::repeat("0")
    //             .take(p as usize)
    //             .collect::<Vec<_>>()
    //             .join(" ")
    //     );
    //     return;
    // }

    let x = Rc::new(X::Var);
    let mut f = (Rc::new(X::Scalar(1)), Rc::new(X::Scalar(0)));
    let g = f.clone();

    for i in 0..p {
        let (u, v) = std::mem::replace(&mut f, g.clone());
        let ui = u.calc(i);
        let vi = v.calc(i);
        let b = (p + a[i as usize] - vi) % p * pow(ui, p - 2, p) % p;
        debug!(i, (ui, vi, b));

        let v = Rc::new(X::Add(
            Rc::new(X::Mul(Rc::clone(&u), Rc::new(X::Scalar(b)))),
            v,
        ));

        let xi = X::Sub(Rc::clone(&x), Rc::new(X::Scalar(i)));
        let u = Rc::new(X::Mul(u, Rc::new(xi)));

        f = (u, v);
    }

    let (u, v) = f;
    let f = X::Add(u, v);
    debug!(f);

    let mut f = f.expand(p);
    while f.len() < p as usize {
        f.push(0);
    }

    println!(
        "{}",
        f.into_iter()
            .map(|b| ((p + b) % p).to_string())
            .collect::<Vec<_>>()
            .join(" ")
    )
}
