#![allow(unused_imports)]
#![allow(non_snake_case)]

use procon::*;
use std::cmp::{max, min};
use std::collections::*;
use std::ops::*;
use std::*;

pub fn main() {
    let (N, C, X, V) = {
        let words: Vec<String> = read_words();
        let N = words[0].parse::<usize>().unwrap();
        let C = words[1].parse::<i64>().unwrap();
        let mut X = Vec::new();
        let mut V = Vec::new();

        // sentinel sushi
        X.push(0);
        V.push(0);

        for _ in 0..N {
            let w = read_words::<String>();
            let x = w[0].parse::<i64>().unwrap();
            let v = w[1].parse::<i64>().unwrap();
            X.push(x);
            V.push(v);
        }

        let N = X.len();

        (N, C, X, V)
    };

    let mut pos = Vec::new();
    {
        let mut x = 0;

        for i in 0..N {
            x += X[i];
            pos.push(x);
        }
    }

    let mut clockwise_score = Vec::new();
    {
        // 一方通行のときのスコア
        let mut max_score1 = 0;

        // 往復のときのスコア
        let mut max_score2 = 0;

        let mut value = 0;
        let mut x = 0;
        let mut d = 0;

        for i in 0..N {
            value += V[i];
            d += X[i] - x;
            x = X[i];

            max_score1 = max(max_score1, value - d);
            max_score2 = max(max_score2, value - d * 2);

            clockwise_score.push((max_score1, max_score2));
        }
    }

    // 反時計回りに最大 j 個の寿司をたべたときの最大スコア
    let mut anticlockwise_score = Vec::new();
    {
        // 一方通行のときのスコア
        let mut max_score1 = 0;

        // 往復のときのスコア
        let mut max_score2 = 0;

        let mut value = 0;
        let mut x = C;
        let mut d = 0;

        for j in (1..N + 1).rev() {
            let j = j % N;
            value += V[j];
            d += x - X[j];
            x = X[j];

            max_score1 = max(max_score1, value - d);
            max_score2 = max(max_score2, value - d * 2);

            anticlockwise_score.push((max_score1, max_score2));
        }
    }

    {
        let mut max_score = 0;

        for i in 0..N {
            let (anticlockwise_score1, anticlockwise_score2) = anticlockwise_score[N - 1 - i];
            let (clockwise_score1, clockwise_score2) = clockwise_score[i];

            // 反時計回りに往復してから時計回りにするか、その逆か
            let score = max(
                anticlockwise_score1 + clockwise_score2,
                anticlockwise_score2 + clockwise_score1,
            );

            max_score = max(max_score, score);
        }

        println!("{}", max_score);
    }

    return;
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
}
