#![allow(unused_imports)]
#![allow(non_snake_case)]

use procon::*;
use std::cmp::{max, min};
use std::collections::*;
use std::ops::*;
use std::*;

fn log2_score(A: i64, B: i64) -> f64 {
    (A as f64).log2() + (B as f64).log2()
}

pub fn calc(A: i64, B: i64) -> i64 {
    let T = A * B;

    /*

    let root = (T as f64).sqrt().floor() as i64;
    let N = (max(1, root - 1)..(root + 4))
        .filter(|&n| n >= 1 && n * n < T)
        .max()
        .unwrap();

    // N : N^2 < T なる最大の整数
    // let M = if N * (N + 1) < T { 2 * N } else { 2 * N - 1 };
    */

    let M = {
        let mut L = 1; // ok
        let mut R = T + 1; // ng

        while R - L > 1 {
            let M = L + (R - L) / 2;

            let getA = |n| {
                if n < A {
                    n
                } else {
                    n + 1
                }
            };
            let getB = |n| {
                let b = (M + 1) - (n - 1);
                if b > B {
                    b
                } else {
                    b - 1
                }
            };

            let m = {
                let mut l = 1; // ok
                let mut r = M + 1; // ng

                while (r - l) > 1 {
                    let m = l + (r - l) / 2;

                    let a1 = getA(m + 1);
                    let b1 = getB(m + 1);
                    let a0 = getA(m);
                    let b0 = getB(m);

                    // a1 * b1 - a0 * b0 >= 0 ?
                    let d = log2_score(a1, b1) - log2_score(a0, b0);

                    if d >= 0.0 {
                        l = m;
                    } else {
                        r = m;
                    }
                }

                l
            };

            let ok = ((m - 10)..(m + 10))
                .filter(|&n| 1 <= n && n <= M)
                .all(|n| log2_score(getA(n), getB(n)) < (T as f64).log2());

            if ok {
                L = M;
            } else {
                R = M;
            }
        }

        L
    };

    M
}

pub fn main() {
    let Q = read_line().parse::<usize>().unwrap();
    for _ in 0..Q {
        let words = read_words::<i64>();
        let A = words[0];
        let B = words[1];
        println!("{}", calc(A, B));
    }
    return;
}

pub mod procon {
    use std;
    use std::collections::*;
    use std::io;
    use std::mem;
    use std::str::FromStr;

    pub fn read_line() -> String {
        let mut buf = String::new();
        io::stdin().read_line(&mut buf).unwrap();
        buf.trim_right().to_owned()
    }

    pub fn read_words<T>() -> Vec<T>
    where
        T: std::str::FromStr,
        T::Err: std::fmt::Debug,
    {
        let mut buf = String::new();
        io::stdin().read_line(&mut buf).unwrap();
        buf.split_whitespace()
            .map(|word| T::from_str(word).unwrap())
            .collect()
    }

    pub fn read_vec(len: usize) -> Vec<String> {
        let mut vec = Vec::new();
        while vec.len() < len {
            let line = read_line();
            for word in line.split_whitespace() {
                vec.push(word.to_owned());
            }
        }
        assert!(vec.len() == len);
        vec
    }

    pub trait IteratorExt: Iterator {
        fn vec(self) -> Vec<Self::Item>
        where
            Self: Sized,
        {
            self.collect()
        }
    }

    impl<T: Iterator> IteratorExt for T {}
}
