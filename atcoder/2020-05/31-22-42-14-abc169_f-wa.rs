//! Framework <https://github.com/vain0x/procon>

#![allow(non_snake_case)]
#![allow(unused_imports)]

use std::{collections::*, mem::swap};

pub struct Scan(Box<dyn Iterator<Item = &'static str>>); // '

impl Scan {
    fn new() -> Self {
        let mut buf = String::new();
        let read_line = move || {
            std::io::stdin().read_line(&mut buf).unwrap();
            Box::leak(buf.split_off(0).into_boxed_str()).split_whitespace()
        };
        Scan(Box::new(std::iter::repeat_with(read_line).flatten()))
    }

    pub fn word<T: std::str::FromStr>(&mut self) -> T {
        self.0.next().unwrap().parse().ok().unwrap()
    }

    pub fn list<T: std::str::FromStr>(&mut self, len: usize) -> Vec<T> {
        std::iter::repeat_with(|| self.word()).take(len).collect()
    }
}

const P: usize = 998244353;

fn main() {
    let mut scan = Scan::new();

    let N = scan.word::<usize>();
    let S = scan.word::<usize>();
    let mut A = scan.list::<usize>(N);
    A.sort();

    let mut dp = vec![vec![0; S + 1]; N + 1];
    let mut freq = vec![0; N + 1];

    dp[0][0] = 1;

    for i in 0..N {
        if A[i] > S {
            break;
        }

        for k in (0..=i).rev() {
            if i + 1 == N {
                dp[k + 1][S] += dp[k][S - A[i]];
                dp[k + 1][S] %= P;
                continue;
            }

            for t in 0..=(S - A[i]) {
                dp[k + 1][t + A[i]] += dp[k][t];
                dp[k + 1][t + A[i]] %= P;
            }
        }
    }

    for k in 0..N {
        freq[k] = dp[k][S];
    }

    #[cfg(debug_assertions)]
    eprintln!("\x1B[33m{}\x1B[0m = {:?}", "freq", freq);

    let mut total = 0;
    let mut h = 1;
    for k in (1..=N).rev() {
        total += freq[k] * h % P;
        total %= P;

        h *= 2;
        h %= P;
    }

    println!("{}", total);
}
