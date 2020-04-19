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
    let A = scan.list::<i64>(N);

    let mut desc = A
        .iter()
        .enumerate()
        .map(|(i, &a)| (a, i))
        .collect::<Vec<_>>();
    desc.sort_by_key(|&(a, i)| (std::cmp::Reverse(a), i));

    let mut dp = vec![vec![0; N + 2]; N + 2];
    // i: すでに配置した人数
    for i in 0..N {
        // x: 左端に配置した人数
        for x in 0..i + 1 {
            let (a, p) = desc[i];

            // 右端に配置するときの位置
            let y = N - (i - x) - 1;

            // なるべく左端に置く
            dp[i + 1][x + 1] = dp[i + 1][x + 1].max(dp[i][x] + a * (p as i64 - x as i64).abs());

            // なるべく右端に置く
            dp[i + 1][x] = dp[i + 1][x].max(dp[i][x] + a * (p as i64 - y as i64).abs());
        }
    }

    let m = (0..N + 1).map(|x| dp[N][x]).max().unwrap();

    println!("{}", m);
}
