//! Framework <https://github.com/vain0x/procon>

#![allow(non_snake_case)]
#![allow(unused_imports)]

use std::{collections::*, ops::Mul};

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

const P: i64 = 1_000_000_007;

fn solve(K: usize, A: &[i64]) -> i64 {
    let N = A.len();

    #[cfg(debug_assertions)]
    eprintln!("\x1B[33m{}\x1B[0m = {:?}", "&A", &A);

    let neg = A.iter().take_while(|&&a| a < 0).count();
    let zero = A[neg..].iter().take_while(|&&a| a == 0).count();
    let pos = N - (neg + zero);

    #[cfg(debug_assertions)]
    eprintln!(
        "\x1B[33m{}\x1B[0m = {:?}",
        "(neg, zero, pos)",
        (neg, zero, pos)
    );

    // 累積和
    let mut left_prod = vec![1; N + 1];
    for i in 0..N {
        left_prod[i + 1] = left_prod[i] * A[i] % P;
    }

    let mut right_prod = vec![1; N + 1];
    for i in (0..N).rev() {
        right_prod[i] = right_prod[i + 1] * A[i] % P;
    }

    #[cfg(debug_assertions)]
    eprintln!("\x1B[33m{}\x1B[0m = {:?}", "&left_prod", &left_prod);

    #[cfg(debug_assertions)]
    eprintln!("\x1B[33m{}\x1B[0m = {:?}", "&right_prod", &right_prod);

    // 負数を偶数回使うケース
    let sl = (K.saturating_sub(pos) + 1) / 2;
    let sr = ((neg.min(K) / 2) + 1).max(sl);

    #[cfg(debug_assertions)]
    eprintln!("\x1B[33m{}\x1B[0m = {:?}", "(sl, sr)", (sl, sr));

    let s_ok = sl < sr;
    let mut s = sl;
    while s + 1 < sr {
        let p = N - (K - s * 2);
        let down = A[p..p + 2].iter().fold(1, Mul::mul);

        let n = s * 2;
        let up = A[n..n + 2].iter().fold(1, Mul::mul);

        #[cfg(debug_assertions)]
        eprintln!(
            "\x1B[33m{}\x1B[0m = {:?}",
            "(p, down, n, up)",
            (p, down, n, up)
        );

        if up < down {
            break;
        }

        s += 1;
    }

    if s_ok {
        #[cfg(debug_assertions)]
        eprintln!("\x1B[33m{}\x1B[0m = {:?}", "s", s);

        let mut t = left_prod[s * 2];
        t *= right_prod[N - (K - (s * 2))];
        t %= P;
        return t;
    }

    // ゼロ
    if zero != 0 {
        return 0;
    }

    // 負数を奇数回使うケース
    if neg >= 1 {
        let neg = neg - 1;
        let K = K - 1;

        let sl = K.saturating_sub(pos).min(neg);
        let sr = (neg.min(K) + 1).max(sl);

        #[cfg(debug_assertions)]
        eprintln!("\x1B[33m{}\x1B[0m = {:?}", "(sl, sr)", (sl, sr));

        let s_ok = sl < sr;
        let mut s = sl;
        while s + 1 < sr {
            let p = neg + zero + (K - s);
            let down = A[p];

            let n = neg - s - 1;
            let up = A[n];

            #[cfg(debug_assertions)]
            eprintln!(
                "\x1B[33m{}\x1B[0m = {:?}",
                "(p, down, n, up)",
                (p, down, n, up)
            );

            if up > down {
                break;
            }

            s += 1;
        }

        if s_ok {
            #[cfg(debug_assertions)]
            eprintln!("\x1B[33m{}\x1B[0m = {:?}", "s", s);

            let mut t = 1;
            for a in &A[neg - s..neg] {
                t *= a;
                t %= P;
            }
            for a in &A[neg + pos..neg + pos + (K - s)] {
                t *= a;
                t %= P;
            }

            t *= A[neg];
            t %= P;
            return t;
        }
    }

    std::i64::MIN
}

fn main() {
    let mut scan = Scan::new();

    let N = scan.word::<usize>();
    let K = scan.word::<usize>();

    let mut A = scan.list::<i64>(N);
    A.sort();

    let n = solve(K, &A);

    #[cfg(debug_assertions)]
    eprintln!("\x1B[33m{}\x1B[0m = {:?}", "n", n);

    println!("{}", (n + P) % P);
}
