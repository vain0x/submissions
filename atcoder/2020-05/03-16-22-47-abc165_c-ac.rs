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

struct Solve {
    N: usize,
    M: usize,
    Q: usize,
    a: Vec<usize>,
    b: Vec<usize>,
    c: Vec<usize>,
    d: Vec<usize>,
    A: Vec<usize>,
}

impl Solve {
    fn new(
        N: usize,
        M: usize,
        Q: usize,
        a: Vec<usize>,
        b: Vec<usize>,
        c: Vec<usize>,
        d: Vec<usize>,
    ) -> Solve {
        Solve {
            N,
            M,
            Q,
            a,
            b,
            c,
            d,
            A: vec![0; N],
        }
    }

    fn solve(&mut self) {
        let M = self.dfs(0, 1);
        println!("{}", M);
    }

    fn dfs(&mut self, i: usize, x: usize) -> usize {
        if i == self.N {
            return self.calc();
        }

        let mut M = 0;

        for y in x..=self.M {
            self.A[i] = y;
            M = M.max(self.dfs(i + 1, y));
        }

        M
    }

    fn calc(&self) -> usize {
        (0..self.Q)
            .map(|i| {
                if self.A[self.b[i]] - self.A[self.a[i]] == self.c[i] {
                    self.d[i]
                } else {
                    0
                }
            })
            .sum()
    }
}

fn main() {
    let mut scan = Scan::new();

    let N = scan.word::<usize>();
    let M = scan.word::<usize>();
    let Q = scan.word::<usize>();
    let mut a = vec![];
    let mut b = vec![];
    let mut c = vec![];
    let mut d = vec![];

    for _ in 0..Q {
        a.push(scan.word::<usize>() - 1);
        b.push(scan.word::<usize>() - 1);
        c.push(scan.word::<usize>());
        d.push(scan.word::<usize>());
    }

    Solve::new(N, M, Q, a, b, c, d).solve();
}
