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

    scan.word::<usize>();
    let S = scan.word::<String>().chars().collect::<Vec<_>>();
    let N = S.len();

    let mut r = 0_usize;
    let mut g = 0_usize;
    let mut b = 0_usize;

    for &c in &S {
        match c {
            'R' => r += 1,
            'G' => g += 1,
            'B' => b += 1,
            _ => {}
        }
    }

    let mut count = r * g * b;

    for i in 0..N {
        for d in 0..N {
            let j = i + d;
            let k = j + d;

            if j < N && k < N && S[i] != S[j] && S[i] != S[k] && S[j] != S[k] {
                count -= 1;
            }
        }
    }

    println!("{}", count);
}
