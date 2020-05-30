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
    let A = scan.list::<usize>(N + 1);

    let mut ok = true;

    // B[d] = (深さ d の内部ノードの個数)
    let mut B = vec![0; N + 1];

    // 最大値を計算する。子ノードの個数が常に1のとき最大。
    for d in (0..N).rev() {
        B[d] = A[d + 1] + B[d + 1];
    }

    // 深さ 0 の頂点は最大 1 個。
    ok = ok && A[0] <= 1;
    B[0] = 1_usize.saturating_sub(A[0]);

    #[cfg(debug_assertions)]
    eprintln!("\x1B[33m{}\x1B[0m = {:?}", "B", B);

    let mut modified;
    while ok {
        modified = false;

        for d in 1..N {
            // 二分木の条件: ある世代のノードの個数の上限は親世代の内部ノードの個数の2倍
            let k = (B[d - 1] * 2).saturating_sub(A[d]);
            if B[d] > k {
                B[d] = k;
                modified = true;
            }
        }

        for d in (0..N).rev() {
            let k = A[d + 1] + B[d + 1];
            if B[d] > k {
                B[d] = k;
                modified = true;
            }
        }

        #[cfg(debug_assertions)]
        eprintln!("\x1B[33m{}\x1B[0m = {:?}", "B", B);

        // 条件を再確認する。
        ok = ok && (0..N).all(|d| B[d] <= A[d + 1] + B[d + 1]);
        ok = ok && (1..=N).all(|d| A[d] + B[d] <= B[d - 1] * 2);

        if !modified {
            break;
        }
    }

    if ok {
        let total = (0..=N).map(|d| A[d] + B[d]).sum::<usize>();
        println!("{}", total);
    } else {
        println!("-1");
    }
}
