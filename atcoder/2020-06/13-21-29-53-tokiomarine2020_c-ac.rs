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
    let mut A = scan.list::<i64>(N);

    A.push(0);
    let mut next = vec![];

    for _ in 0..K {
        next.clear();
        next.resize(N + 1, 0);

        for i in 0..N {
            let l = ((i as i64) - A[i]).max(0) as usize;
            let r = ((i as i64) + A[i] + 1).min(N as i64) as usize;
            next[l] += 1_i64;
            next[r] -= 1;
        }

        let mut m = next[0];
        for i in 0..N {
            next[i + 1] += next[i];
            m = m.min(next[i]);
        }

        std::mem::swap(&mut A, &mut next);

        if m == N as i64 {
            break;
        }
    }

    println!(
        "{}",
        A[..N]
            .iter()
            .map(|&a| a.to_string())
            .collect::<Vec<_>>()
            .join(" ")
    )
}
