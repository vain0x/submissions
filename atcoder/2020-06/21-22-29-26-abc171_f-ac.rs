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

const P: i64 = 1_000_000_007;

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

pub struct ChooseFn {
    fact: Vec<i64>,
    fact_inv: Vec<i64>,
}

impl ChooseFn {
    pub fn new(m: usize) -> Self {
        let mut fact = vec![0; m];
        fact[0] = 1;
        fact[1] = 1;
        for i in 2..m {
            fact[i] = (i as i64) * fact[i - 1] % P;
        }

        let mut fact_inv = vec![0; m];
        fact_inv[m - 1] = pow(fact[m - 1], P - 2);
        for i in (1..m).rev() {
            fact_inv[i - 1] = (i as i64) * fact_inv[i] % P;
        }

        ChooseFn { fact, fact_inv }
    }

    pub fn call(&self, n: usize, r: usize) -> i64 {
        if n < r {
            return 0;
        }

        let mut t = self.fact[n];
        t *= self.fact_inv[n - r];
        t %= P;
        t *= self.fact_inv[r];
        t % P
    }
}

fn main() {
    let mut scan = Scan::new();

    let K = scan.word::<usize>();
    let S = scan.word::<String>();

    let choose = ChooseFn::new(K + S.len() + 1);

    let mut total = 0;

    for k in 0..=K {
        let mut n = pow(26, (K - k) as i64);
        n *= pow(25, k as i64);
        n %= P;
        n *= choose.call(S.len() + k - 1, k);

        total += n % P;
        total %= P;
    }

    println!("{}", total);
}
