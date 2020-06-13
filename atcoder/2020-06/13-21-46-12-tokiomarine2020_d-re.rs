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
    let mut V = vec![0; N];
    let mut W = vec![0; N];

    for i in 0..N {
        V[i] = scan.word::<usize>();
        W[i] = scan.word::<usize>();
    }

    let mut a = vec![];
    let mut dp = vec![vec![]; 100000];

    let Q = scan.word::<usize>();

    for _ in 0..Q {
        let v = scan.word::<usize>() - 1;
        let L = scan.word::<usize>();

        a.clear();

        let mut u = v;
        loop {
            a.push(u);
            if u == 0 {
                break;
            }
            u = (u - 1) / 2;
        }

        let n = a.len();
        for w in 0..=L {
            dp[w].clear();
            dp[w].resize(n + 1, 0);
        }

        for i in 0..n {
            for w in 0..=L {
                dp[w][i + 1] = dp[w][i + 1].max(dp[w][i]);

                if w + W[a[i]] <= L {
                    dp[w + W[a[i]]][i + 1] = dp[w + W[a[i]]][i + 1].max(dp[w][i] + V[a[i]]);
                }
            }
        }

        println!("{}", (0..=L).map(|w| dp[w][n]).max().unwrap());
    }
}
