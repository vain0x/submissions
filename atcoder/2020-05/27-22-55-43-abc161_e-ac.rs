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
    let K = scan.word::<usize>();
    let C = scan.word::<usize>();
    let S = scan.word::<String>().chars().collect::<Vec<_>>();

    // 前から貪欲
    let mut work1 = vec![false; N];
    let mut k1 = 0;
    let mut i = 0;
    while i < N {
        if S[i] == 'o' {
            work1[i] = true;
            k1 += 1;
            i += 1 + C;
        } else {
            i += 1;
        }
    }

    // 後ろから貪欲
    let mut work2 = vec![false; N];
    let mut k2 = 0;
    let mut i = N;
    while i >= 1 {
        i -= 1;

        if S[i] == 'o' {
            work2[i] = true;
            k2 += 1;
            i = i.saturating_sub(C);
        }
    }

    if k1 == K && k2 == K {
        for i in 0..N {
            if work1[i] && work2[i] {
                println!("{}", i + 1);
            }
        }
    }
}
