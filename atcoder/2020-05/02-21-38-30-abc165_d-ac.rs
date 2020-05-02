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

fn f(A: i64, B: i64, x: i64) -> i64 {
    (A * x / B) - A * (x / B)
}

fn main() {
    let mut scan = Scan::new();

    let A = scan.word::<i64>();
    let B = scan.word::<i64>();
    let N = scan.word::<i64>();

    let M = if B - 1 <= N {
        f(A, B, B - 1)
    } else {
        f(A, B, N)
    };

    println!("{}", M)
}
