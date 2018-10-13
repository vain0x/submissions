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

const P: i64 = 1_000_000_007;

/// Calcuates `x^n`. O(log n) time.
/// By Fermat's little theorem, `x^(-1) = pow(x, P - 2)`.
pub fn pow(x: i64, n: i64) -> i64 {
    let (mut x, mut y, mut n) = (x % P, 1_i64, n);
    while n > 0 {
        if n % 2 != 0 {
            y = (y * x) % P;
            n -= 1;
        }

        x = (x * x) % P;
        n /= 2;
    }
    y
}

fn main() {
    let N = read!(usize);
    let A = read![[i64]];

    let nf = (2..(N + 1) as i64).fold(1_i64, |x, y| x * y % P);

    // h[k] = Σ[i≤k] 1/i
    let mut h = vec![0; N + 1];
    for i in 1..h.len() {
        h[i] += h[i - 1];
        h[i] %= P;
        h[i] += pow(i as i64, P - 2);
        h[i] %= P;
    }

    // for i in 1..5 {
    //     let a = (1..i + 1)
    //         .map(|i| pow(i as i64, P - 2))
    //         .fold(0, |x, y| (x + y) % P);
    //     debug!((i, h[i], a));
    // }

    let mut s = 0_i64;
    for i in 0..N {
        let mut t = 0_i64;

        // j 番目のブロックを取ろうとしたときにブロック i の重さが加算される確率の和
        // for j in 0..N {
        //     t += pow((j as i64 - i as i64).abs() + 1, P - 2);
        //     t %= P;
        // }

        // Σ[0≤j<N] (1/(|i-j|+1))
        // = 1 + Σ[0≤j<i] (1 / (i-j+1)) + Σ[i<j<N] (1 / (j-i+1))
        // j < i のとき k=i-j+1 とおくと 0 ≤ j=i-k+1 < i だから 1 < k ≤ i+1
        // i < j のとき k=j-i+1 とおくと i < j=i+k-1 < N だから 1 < k < N-i+1

        t += h[i + 1];
        t %= P;
        t += h[N - i];
        t %= P;
        t += P - 2 + 1;
        t %= P;

        s += A[i] % P * t % P;
        s %= P;
    }
    s *= nf;
    s %= P;

    println!("{}", s)
}
