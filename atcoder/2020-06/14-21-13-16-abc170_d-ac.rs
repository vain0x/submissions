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
    let A = scan.list::<usize>(N);

    let mut freq = HashMap::new();
    for &a in &A {
        *freq.entry(a).or_insert(0) += 1;
    }

    let mut b = freq.keys().collect::<Vec<_>>();
    b.sort();

    let m = b.last().copied().unwrap() + 1;
    let mut done = vec![false; m + 1];
    for (&a, &k) in &freq {
        if k >= 2 {
            done[a] = true;
        }
    }

    for &x in &b {
        let mut k = 2;
        while k * x <= m {
            done[k * x] = true;
            k += 1;
        }
    }

    let n = A.iter().filter(|&&a| !done[a]).count();
    println!("{}", n);
}
