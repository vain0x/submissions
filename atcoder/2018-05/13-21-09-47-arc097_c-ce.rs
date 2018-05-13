#![allow(unused_imports)]
#![allow(non_snake_case)]

use procon::*;
use std::collections::*;
use std::ops::*;
use std::*;

#[derive(PartialEq, Clone, Copy, Debug)]
enum Color {
    W,
    B,
}

#[derive(PartialEq, Clone, Debug)]
struct Ball {
    pub color: Color,
    pub rank: usize,
}

pub fn main() {
    let N: usize = read_line().parse().unwrap();
    let balls = (0..(N * 2))
        .map(|_| {
            let w: Vec<String> = read_words();
            let c = if w[0] == "W" { Color::W } else { Color::B };
            let r = w[1].parse().unwrap();
            Ball { color: c, rank: r }
        })
        .collect::<Vec<_>>();

    let count = {
        let f = |color: Color| {
            // ks[i][rank]: (ある色について) 位置 i より右にある rank 以下の玉の個数
            let mut ks = (0..(2 * N)).map(|_| vec![0; N + 1]).collect::<Vec<_>>();
            for rank in 1..N + 1 {
                let mut k = 0;
                for i in (0..(2 * N)).rev() {
                    if balls[i].color == color && balls[i].rank <= rank {
                        k += 1;
                    }
                    ks[i][rank] = k;
                }
            }
            ks
        };

        let ws = f(Color::W);
        let bs = f(Color::B);

        let g = |color: Color| {
            let mut indexes = vec![0; N + 1];
            for i in 0..(2 * N) {
                if balls[i].color == color {
                    indexes[balls[i].rank] = i;
                }
            }
            indexes
        };

        let wi = g(Color::W);
        let bi = g(Color::B);

        move |next: &Ball, w: usize, b: usize| {
            let i = if next.color == Color::W {
                wi[next.rank]
            } else {
                bi[next.rank]
            };
            ws[i][w] + bs[i][b]
        }
    };

    let mut dp = vec![vec![std::usize::MAX; N + 1]; N + 1];
    dp[0][0] = 0;

    for w in 0..N + 1 {
        for b in 0..N + 1 {
            // 次が W(w+1) のケース
            // 1以上w以下のボールが左側に配置されている
            // これらのうちもともと W(w+1) より右側にあったものの数が転倒数
            if w + 1 <= N {
                let ball = &Ball {
                    color: Color::W,
                    rank: w + 1,
                };
                let k1 = count(ball, w, b);
                dp[w + 1][b] = dp[w + 1][b].min(dp[w][b] + k1);
            }

            if b + 1 <= N {
                let ball = &Ball {
                    color: Color::B,
                    rank: b + 1,
                };
                let k2 = count(ball, w, b);
                dp[w][b + 1] = dp[w][b + 1].min(dp[w][b] + k2);
            }
        }
    }

    println!("{}", dp[N][N]);
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
