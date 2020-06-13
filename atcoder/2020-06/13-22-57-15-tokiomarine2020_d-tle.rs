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

#[derive(Default)]
struct Solve {
    N: usize,
    V: Vec<usize>,
    W: Vec<usize>,
    Q: usize,
    v: Vec<usize>,
    L: Vec<usize>,
    dp: Vec<Vec<usize>>,
    qs: Vec<Vec<(usize, usize)>>,
    rs: Vec<usize>,
    ls: Vec<usize>,
}

impl Solve {
    fn solve(mut self) {
        let N = self.N;
        let Q = self.Q;

        self.rs.resize(Q, 0);
        self.qs.resize(N, vec![]);
        self.ls.resize(N, 0);

        // let mut ml = 0;

        for q in 0..Q {
            let u = self.v[q];
            let l = self.L[q];

            self.qs[u].push((l, q));
            self.ls[u] = self.ls[u].max(l);
            // ml = ml.max(l);
        }

        // self.ls.resize(N, ml);
        self.dfs_l(0);

        self.dfs_r(0);

        for q in 0..Q {
            println!("{}", self.rs[q]);
        }
    }

    fn dfs_l(&mut self, u: usize) -> usize {
        if u >= self.N {
            return 0;
        }

        if u >= 1 {
            self.ls[u] = self.ls[u].max(self.ls[(u - 1) / 2]);
        }

        let w = self.ls[u]
            .max(self.dfs_l(u * 2 + 1))
            .max(self.dfs_l(u * 2 + 2));
        self.ls[u] = w;
        w
    }

    fn dfs_r(&mut self, u: usize) {
        if u >= self.N {
            return;
        }

        let l = self.ls[u];
        let h = self.dp.len();
        self.dp.push(vec![0; l + 1]);

        if u == 0 {
            if self.W[u] <= l {
                self.dp[h][self.W[u]] += self.V[u];
            }
        } else {
            for w in 0..=l {
                self.dp[h][w] = self.dp[h][w].max(self.dp[h - 1][w]);

                if w + self.W[u] <= l {
                    self.dp[h][w + self.W[u]] =
                        self.dp[h][w + self.W[u]].max(self.dp[h - 1][w] + self.V[u]);
                }
            }
        }

        for w in 1..=l {
            self.dp[h][w] = self.dp[h][w].max(self.dp[h][w - 1]);
        }

        for &(w, q) in &self.qs[u] {
            self.rs[q] = self.dp[h][w];
        }

        self.dfs_r(u * 2 + 1);
        self.dfs_r(u * 2 + 2);

        self.dp.pop();
    }
}

fn main() {
    let mut scan = Scan::new();

    let N = scan.word::<usize>();
    let mut V = vec![0; N];
    let mut W = vec![0; N];

    for i in 0..N {
        V[i] = scan.word::<usize>();
        W[i] = scan.word::<usize>();
    }

    let Q = scan.word::<usize>();
    let mut v = vec![0; Q];
    let mut L = vec![0; Q];

    for q in 0..Q {
        v[q] = scan.word::<usize>() - 1;
        L[q] = scan.word::<usize>();
    }

    Solve {
        N,
        V,
        W,
        Q,
        v,
        L,
        ..Solve::default()
    }
    .solve();
}
