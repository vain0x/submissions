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
    let H = scan.list::<usize>(N);

    let mut bad = vec![false; N];
    for _ in 0..M {
        let u = scan.word::<usize>() - 1;
        let v = scan.word::<usize>() - 1;

        if H[u] <= H[v] {
            bad[u] = true;
        }
        if H[v] <= H[u] {
            bad[v] = true;
        }
    }

    let count = (0..N).filter(|&u| !bad[u]).count();

    println!("{}", count);
}
