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

// editorial の解法

const P: usize = 998244353;

fn main() {
    let mut scan = Scan::new();

    let N = scan.word::<usize>();
    let S = scan.word::<usize>();
    let A = scan.list::<usize>(N);

    // dp[i][x] = n
    //    T を 0..i の部分集合とし、U を T の部分集合とする。
    //    ΣA(j) (j∈U) = x となる。
    //    このような状態の数が n
    let mut dp = vec![vec![0; S + 1]; N + 1];

    // i = 0 のとき T = U = {} しかありえない。
    dp[0][0] = 1_usize;

    for i in 0..N {
        for x in 0..=S {
            // i を T, U の両方に入れる。
            if x + A[i] <= S {
                dp[i + 1][x + A[i]] += dp[i][x];
            }

            // i を T に入れて、U には入れない。
            // または、i を T にも U にも入れない。(遷移先は同じ。)
            dp[i + 1][x] += dp[i][x] * 2;
        }

        for x in 0..=S {
            dp[i + 1][x] %= P;
        }
    }

    println!("{}", dp[N][S]);
}
