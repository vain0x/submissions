#![allow(unused_imports)]
#![allow(non_snake_case)]

use procon::*;
use std::cmp::{max, min};
use std::collections::*;
use std::ops::*;
use std::*;

fn floor_sqrt(T: i64) -> i64 {
    let mut L = 1;
    let mut R = T + 1;
    while R - L > 1 {
        let M = L + (R - L) / 2;

        // M * M < T ?
        let ok = {
            let q = T / M;
            let r = T % M;
            q == M && r > 0 || q > M
        };
        if ok {
            L = M;
        } else {
            R = M;
        }
    }
    L
}

pub fn calc(A: i64, B: i64) -> i64 {
    assert!(A <= B);

    // 高橋くんのスコア
    let T = A * B;

    // C: C^2 < T なる最大の整数
    let C = floor_sqrt(T);

    // ある参加者のコンテストの順位が A, B だったということを (A, B) で表す。
    // スコアが T 未満の参加者のスコアを S = A'*B' とおく。

    if A == B {
        // A'*B' < A^2 から A' < A または B' < A がいえて、 A' < A は最大 A-1 人、B' < A は最大 A-1 人、ゆえに最大値の上限は 2A - 2.
        // (1, 2A - 2), (2, 2A - 1), ... (A - 1, A - 1), ..., (2A - 2,  1) という結果がありうるのでこの上限が最大値になる。
        2 * A - 2
    } else if (B - A) == 1 {
        // A'*B' < A(A + 1) ゆえ A' <= A.
        // A' = A はありえないので (A位は高橋くんだから) 、 A' < A または B' <= A。
        // A' < A は A-1 人。
        // B' == A のとき A' < A なので A-1人。
        // 実際、 (1, 2A - 1), (2, 2A - 2), ..., (A, A), ..., (2A - 1, 1) という結果があるので上限が最大値になる。
        2 * A - 1
    } else if C * (C + 1) >= T {
        // このとき |A - B| >= 2 である。
        // C(C+1) >= AB とする。
        // A < C < C+1 < B である。(B <= C+1 なら (A <= B - 2 <= C - 1 なので) AB <= (C - 1)(C + 1) < C^2 < AB となり矛盾。)
        // A' <= C or B' <= C である。
        // (C - 1) + (C - 1) = 2C - 2 が上限になる?
        // 実際、次の2種類の結果の組み合わせの総和が 2C - 2 個なので上限になる。
        //  { (A - 1 - n, B + 1 + n) | 0 <= n <= A - 2 } (A - 2個)
        //  { (2C - n, n) | 1 <= n <= 2C - A - 1 } (2C - A - 1個)
        2 * C - 2
    } else {
        // このとき |A - B| >= 2, C(C+1) < T 。
        // A' == C+1 and B' <= C または A' <=C and B' <= C でなければならない。
        // (C - 1) + C = 2C が上限になる。
        // 実際、 (1, 2C), (2, 2C - 1), ... (C, C + 1), ..., (2C, 1) がありうるのでOK
        2 * C - 1
    }
}

pub fn main() {
    let Q = read_line().parse::<usize>().unwrap();
    for _ in 0..Q {
        let words = read_words::<i64>();
        let A = words[0];
        let B = words[1];
        println!("{}", calc(min(A, B), max(A, B)));
    }
    return;
}

#[test]
fn test_floor_sqrt() {
    assert_eq!(floor_sqrt(15), 3);
    assert_eq!(floor_sqrt(16), 3);
    assert_eq!(floor_sqrt(17), 4);
}

pub mod procon {
    use std;
    use std::collections::*;
    use std::io;
    use std::mem;
    use std::str::FromStr;

    pub fn read_line() -> String {
        let mut buf = String::new();
        io::stdin().read_line(&mut buf).unwrap();
        buf.trim_right().to_owned()
    }

    pub fn read_words<T>() -> Vec<T>
    where
        T: std::str::FromStr,
        T::Err: std::fmt::Debug,
    {
        let mut buf = String::new();
        io::stdin().read_line(&mut buf).unwrap();
        buf.split_whitespace()
            .map(|word| T::from_str(word).unwrap())
            .collect()
    }

    pub fn read_vec(len: usize) -> Vec<String> {
        let mut vec = Vec::new();
        while vec.len() < len {
            let line = read_line();
            for word in line.split_whitespace() {
                vec.push(word.to_owned());
            }
        }
        assert!(vec.len() == len);
        vec
    }

    pub trait IteratorExt: Iterator {
        fn vec(self) -> Vec<Self::Item>
        where
            Self: Sized,
        {
            self.collect()
        }
    }

    impl<T: Iterator> IteratorExt for T {}
}
