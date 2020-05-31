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

fn median(x: &[i64]) -> i64 {
    let n = x.len();

    let mut x = x.to_owned();
    x.sort();

    if n % 2 == 0 {
        (x[n / 2 - 1] + x[n / 2]) / 2
    } else {
        x[n / 2]
    }
}

fn main() {
    let mut scan = Scan::new();

    let N = scan.word::<usize>();
    let mut A = vec![];
    let mut B = vec![];

    for _ in 0..N {
        let a = scan.word::<i64>();
        let b = scan.word::<i64>();

        let (a, b) = if N % 2 == 0 { (a * 2, b * 2) } else { (a, b) };

        A.push(a);
        B.push(b);
    }

    let am = median(&A);
    let bm = median(&B);

    let n = (bm - am + 1).max(0);

    println!("{}", n);
}
