//! Framework <https://github.com/vain0x/procon>

#![allow(non_snake_case)]
#![allow(unused_imports)]

use std::collections::*;

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

const P: i64 = 998244353;

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
    let mut scan = Scan::new();

    let N = scan.word::<usize>();
    let M = scan.word::<usize>();
    let K = scan.word::<usize>();

    let mut fact = vec![0; N + 1];
    fact[0] = 1;
    fact[1] = 1;
    for i in 2..fact.len() {
        fact[i] = i as i64 * fact[i - 1] % P;
    }

    let mut fact_inv = vec![0; N + 1];
    fact_inv[N] = pow(fact[N], P - 2);
    for i in (1..fact_inv.len()).rev() {
        fact_inv[i - 1] = i as i64 * fact_inv[i] % P;
    }

    let mut x = 0;

    for k in 0..=K {
        let mut y = fact[N - 1];
        y *= fact_inv[N - 1 - k];
        y %= P;
        y *= fact_inv[k];
        y %= P;
        y *= pow((M - 1) as i64, (N - 1 - k) as i64);
        y %= P;

        x += y;
        x %= P;
    }

    x *= M as i64;
    x %= P;

    println!("{}", x);
}
