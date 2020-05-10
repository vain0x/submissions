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
    let X = scan.word::<i64>();

    let mut T = vec![];
    for _ in 0..N {
        let c = scan.word::<i64>();
        let A = scan.list::<i64>(M);
        T.push((c, A));
    }

    let mut m = std::i64::MAX;
    let mut Y = vec![0; M];

    for s in 0..1 << N {
        let mut cost = 0;
        Y.clear();
        Y.resize(M, 0);

        for i in 0..N {
            if (s >> i) & 1 != 0 {
                cost += T[i].0;

                for j in 0..M {
                    Y[j] += T[i].1[j];
                }
            }
        }

        if Y.iter().all(|&y| y >= X) {
            m = m.min(cost);
        }
    }

    if m == std::i64::MAX {
        println!("-1");
    } else {
        println!("{}", m);
    }
}
