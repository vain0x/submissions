//! Framework <https://github.com/vain0x/procon>

#![allow(non_snake_case)]
#![allow(unused_imports)]

use std::cmp::{max, min};
use std::collections::*;

pub struct Scan(Box<Iterator<Item = &'static str>>); // '

impl Scan {
    fn new() -> Self {
        let mut buf = String::new();
        let mut read_line = move || {
            buf.clear();
            std::io::stdin().read_line(&mut buf).unwrap();
            Box::leak(buf.clone().into_boxed_str()).split_whitespace()
        };
        Scan(Box::new((0..).map(move |_| read_line()).flatten()))
    }

    pub fn word<T: std::str::FromStr>(&mut self) -> T {
        self.0.next().unwrap().parse().ok().unwrap()
    }

    pub fn list<T: std::str::FromStr>(&mut self, len: usize) -> Vec<T> {
        (0..len).map(|_| self.word()).collect()
    }
}

/// ビット数
const H: usize = 50;

fn main() {
    let mut scan = Scan::new();

    let N = scan.word::<usize>();
    let K = scan.word::<usize>();
    let A = scan.list::<i64>(N);

    // sum[h][b] = s
    // X の上から h ビット目を b にしたとき、(X ^ A[i]) のそのビットの総和が s に等しい
    let mut sum = vec![vec![0_i64; 2]; H];

    for &a in &A {
        for h in 0..H {
            sum[h][0] += a & (1 << (H - h - 1));
            sum[h][1] += (!a) & (1 << (H - h - 1));
        }
    }

    // dp[h][eq] = m
    //
    // 状態:
    // - X の上位 h ビットが確定済み。
    // - eq = 1 <=> X と K の上位 h ビットが一致
    //
    // 計算値:
    // - m はこの状態における (X ^ A[i]) の上位 h ビットの総和の最大値
    let mut dp = vec![vec![0_i64; 2]; H + 1];

    // ok[h][eq] = (状態 (h, eq) に遷移可能？)
    let mut ok = vec![vec![false; 2]; H + 1];
    ok[0][1] = true;

    for h in 0..H {
        if ok[h][0] {
            for b in 0..2 {
                dp[h + 1][0] = max(dp[h + 1][0], dp[h][0] + sum[h][b]);
                ok[h + 1][0] = true;
            }
        }

        if ok[h][1] {
            let kb = (K >> (H - h - 1)) & 1;
            if kb == 1 {
                // K 未満のビット (0) を立てるケース
                dp[h + 1][0] = max(dp[h + 1][0], dp[h][1] + sum[h][0]);
                ok[h + 1][0] = true;
            }

            // K と同じビットを立てるケース
            dp[h + 1][1] = max(dp[h + 1][1], dp[h][1] + sum[h][kb]);
            ok[h + 1][1] = true;
        }
    }

    println!("{}", max(dp[H][0], dp[H][1]));
}
