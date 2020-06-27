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

fn main() {
    let mut scan = Scan::new();

    let N = scan.word::<usize>();
    let M = scan.word::<usize>();
    let K = scan.word::<usize>();
    let A = scan.list::<usize>(N);
    let B = scan.list::<usize>(M);

    let mut ai = 0;
    let mut bi = 0;
    let mut n = 0;
    let mut total = 0;

    while total <= K && ai < A.len() && bi < B.len() {
        let t = if A[ai] < B[bi] {
            ai += 1;
            A[ai - 1]
        } else {
            bi += 1;
            B[bi - 1]
        };

        if total + t > K {
            break;
        }

        total += t;
        n += 1;
    }

    while ai < A.len() && total + A[ai] <= K {
        total += A[ai];
        ai += 1;
        n += 1;
    }

    while bi < B.len() && total + B[bi] <= K {
        total += B[bi];
        bi += 1;
        n += 1;
    }

    println!("{}", n);
}
