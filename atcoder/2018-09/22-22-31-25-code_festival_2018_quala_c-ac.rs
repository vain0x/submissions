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

const P: i64 = 1_000_000_007;

#[allow(unused)]
fn pow(mut x: i64, mut n: i64) -> i64 {
    let mut y = 1_i64;
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

// K個のものをN分割する場合の数
#[allow(unused)]
fn hcombo(N: usize, K: i64, inv: &Vec<i64>) -> i64 {
    let mut x = 1_i64;
    for i in 0..(N - 1) {
        x = (x * (K + (N - 1 - i) as i64) % P * inv[1 + i]) % P;
    }
    x
}

fn hi(mut x: i64) -> usize {
    let mut h = 0;
    while x > 0 {
        x /= 2;
        h += 1;
    }
    h
}

fn main() {
    let (_, K) = read!(usize, i64);
    let A = read![[i64]];

    // let mut inv = vec![1_i64; A.len() + 1];
    // for i in 2..inv.len() {
    //     inv[i] = pow(i as i64, P - 2);
    // }

    // let mut b = K;

    // for (i, &a) in A.iter().enumerate() {
    //     let h = hi(a) as i64;
    //     if h < b {
    //         b = h;
    //     }

    // debug!((i, a, h, b));
    // }

    // let c = hcombo(A.len(), b, &inv);

    let H = A.iter().map(|&a| hi(a)).vec();
    let m = H.iter().cloned().sum::<usize>();
    let m = if K > m as i64 {
        m + 1
    } else {
        min(K as usize, m)
    };
    let N = A.len();

    // debug!(N, m, H);

    struct Calc {
        A: Vec<i64>,
        H: Vec<usize>,
        dp: Vec<Vec<Vec<i64>>>,
    }

    impl Calc {
        fn calc(&mut self, i: usize, m: usize, just: usize) -> i64 {
            if i == self.A.len() {
                if just != 0 && m > 0 {
                    return 0;
                }
                return 1_i64;
            }

            if self.dp[i][m][just] != std::i64::MAX {
                return self.dp[i][m][just];
            }

            let mut s = 0_i64;
            for h in 0..min(m, self.H[i]) + 1 {
                let just = if just != 0 && h < self.H[i] { 1 } else { 0 };
                s = (s + self.calc(i + 1, m - h, just)) % P;
            }

            self.dp[i][m][just] = s;
            // debug!((i, m, s));
            s
        }
    }

    let mut calc = Calc {
        A: A,
        H: H,
        dp: vec![vec![vec![std::i64::MAX; 2]; m + 1]; N + 1],
    };

    let c = calc.calc(0, m, 1);

    println!("{}", c)
}
