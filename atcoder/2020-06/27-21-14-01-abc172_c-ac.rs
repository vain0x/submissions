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

pub fn upper_bound<T: Ord>(xs: &[T], y: &T) -> usize {
    let mut l = 0;
    let mut r = xs.len() + 1;

    while r - l > 1 {
        let m = l + (r - l) / 2;
        if &xs[m - 1] <= y {
            l = m;
        } else {
            r = m;
        }
    }

    l
}

fn main() {
    let mut scan = Scan::new();

    let N = scan.word::<usize>();
    let M = scan.word::<usize>();
    let K = scan.word::<usize>();
    let A = scan.list::<usize>(N);
    let B = scan.list::<usize>(M);

    let mut bb = B.clone();
    bb.insert(0, 0);
    for i in 0..M {
        bb[i + 1] += bb[i];
    }

    let mut at = 0;
    let mut max_n = 0;
    for ai in 0..=N {
        if at > K {
            break;
        }

        let bi = upper_bound(&bb, &K.saturating_sub(at));
        max_n = max_n.max(ai + bi.saturating_sub(1));

        if ai < N {
            at += A[ai];
        }
    }

    println!("{}", max_n);
}
