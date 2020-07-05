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

pub fn lower_bound<T: Ord>(xs: &[T], y: &T) -> usize {
    let mut l = 0;
    let mut r = xs.len() + 1;

    while r - l > 1 {
        let m = l + (r - l) / 2;
        if &xs[m - 1] < y {
            l = m;
        } else {
            r = m;
        }
    }

    l
}

pub fn upper_bound<T: Ord>(xs: &[T], y: &T) -> usize {
    let mut l = 0;
    let mut r = xs.len() + 1;

    while r - l > 1 {
        let m = l + (r - l) / 2;
        if &xs[m - 1] <= y {
            l = m;
        } else {
            r = m;
        }
    }

    l
}

const P: i64 = 1_000_000_007;

fn mul(x: i64, y: &i64) -> i64 {
    (x * (*y)) % P
}

fn solve(K: usize, A: &[i64]) -> i64 {
    let N = A.len();

    #[cfg(debug_assertions)]
    eprintln!("\x1B[33m{}\x1B[0m = {:?}", "&A", &A);

    let neg = lower_bound(A, &0);
    let zero = upper_bound(A, &0) - neg;
    let nonpos = neg + zero;
    let pos = N - nonpos;

    #[cfg(debug_assertions)]
    eprintln!(
        "\x1B[33m{}\x1B[0m = {:?}",
        "(neg, zero, pos)",
        (neg, zero, pos)
    );

    // 正の数をなるべく多く使うときに必要な負数の個数の半分
    let sl = (K.saturating_sub(pos) + 1) / 2;
    // 正の数をなるべく少なく使うときに必要な負数の個数の半分
    let sr = neg.min(K) / 2;

    #[cfg(debug_assertions)]
    eprintln!("\x1B[33m{}\x1B[0m = {:?}", "(sl, sr)", (sl, sr));

    // 結果を正にできる？
    let is_pos = sl <= sr;
    if is_pos {
        let mut s = sl;
        while s + 1 <= sr {
            // s += 1 で失われる正の数
            let p = N - (K - s * 2);
            let down = A[p - 2..p].iter().fold(1, Mul::mul);

            // s += 1 で得られる負の数
            let n = s * 2;
            let up = A[n..n + 2].iter().fold(1, Mul::mul);

            #[cfg(debug_assertions)]
            eprintln!(
                "\x1B[33m{}\x1B[0m = {:?}",
                "(p, down, n, up)",
                (p, down, n, up)
            );

            if up >= down {
                s += 1;
                continue;
            }

            break;
        }

        #[cfg(debug_assertions)]
        eprintln!("\x1B[33m{}\x1B[0m = {:?}", "s", s);

        return A[..s * 2]
            .iter()
            .chain(&A[N - (K - s * 2)..])
            .fold(1_i64, mul);
    }

    if zero != 0 {
        return 0;
    }

    // 以下、負数を奇数回使って結果が負になるケース
    assert!(neg >= 1);

    // 絶対値を最小化したいので、負数のうち絶対値が最小のものを選ぶことは確定。
    let neg = neg - 1;
    let K = K - 1;

    let sl = K.saturating_sub(pos).min(neg);
    let sr = neg.min(K);
    assert!(sl <= sr);

    #[cfg(debug_assertions)]
    eprintln!("\x1B[33m{}\x1B[0m = {:?}", "(sl, sr)", (sl, sr));

    let mut s = sl;
    while s + 1 <= sr {
        let p = nonpos + (K - s);
        let down = A[p];

        let n = neg - s - 1;
        let up = A[n];

        #[cfg(debug_assertions)]
        eprintln!(
            "\x1B[33m{}\x1B[0m = {:?}",
            "(p, down, n, up)",
            (p, down, n, up)
        );

        if up <= down {
            s += 1;
            continue;
        }

        break;
    }

    #[cfg(debug_assertions)]
    eprintln!("\x1B[33m{}\x1B[0m = {:?}", "s", s);

    A[neg - s..neg + 1]
        .iter()
        .chain(&A[neg + zero..neg + zero + (K - s)])
        .fold(1_i64, mul)
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
