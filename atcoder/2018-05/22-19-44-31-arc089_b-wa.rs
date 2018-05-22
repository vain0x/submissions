#![allow(unused_imports)]
#![allow(non_snake_case)]

use procon::*;
use std::cmp::{max, min};
use std::collections::*;
use std::ops::*;
use std::*;

#[derive(PartialEq, Clone, Copy, Debug)]
enum Color {
    B,
    W,
}

pub fn main() {
    let (_, k, w, board) = {
        let words = read_words::<usize>();
        let (N, K) = (words[0], words[1]);
        let w = K * 2;

        // board[y][x] = d: score +d if black
        let mut board = vec![vec![0_i32; w]; w];

        for _ in 0..N {
            let words = read_words::<String>();
            let (x, y, c) = (
                words[0].parse::<i32>().unwrap(),
                words[1].parse::<i32>().unwrap(),
                if words[2] == "B" { Color::B } else { Color::W },
            );

            let W = w as i32;
            let y = ((y % W + W) % W) as usize;
            let x = ((x % W + W) % W) as usize;

            board[y][x] += if c == Color::B { 1 } else { -1 };
        }
        (N, K, w, board)
    };

    // let transposed = {
    //     let mut t = vec![vec![0; w]; w];
    //     for y in 0..w {
    //         for x in 0..w {
    //             t[(w - x) % w][y] = board[y][x];
    //         }
    //     }
    //     t
    // };

    let mut score = 0;
    let mut up = vec![vec![0_i32; w]; w];
    let mut uy = vec![0_i32; w];
    let mut ux = vec![0_i32; w];
    for y in 0..w {
        for x in 0..w {
            let d = board[y][x];

            // 黒なら初期状態で貢献
            let black = (y < k) == (x < k);
            let d = if black { d } else { -d };

            // 色が一致していればプラス
            if d > 0 {
                score += d;
            }

            let ty = y % k;
            let tx = x % k;

            // 模様が右に (tx + 1) シフトしたら貢献が消える (スコアが減る)、
            // さらに k シフトしたら再び貢献する
            {
                ux[(tx + 1) % w] -= d;
                ux[(tx + 1 + k) % w] += d;
            }

            {
                uy[(ty + 1) % w] -= d;
                uy[(ty + 1 + k) % w] += d;
            }

            // 模様が上に (ty + 1) シフトしたら色が変わって貢献が逆転する、
            // さらに k シフトしたら戻る
            {
                up[(ty + 1) % w][(tx + 1) % w] += 2 * d;
                up[(ty + 1) % w][(tx + 1 + k) % w] -= 2 * d;

                up[(ty + 1 + k) % w][(tx + 1) % w] -= 2 * d;
                up[(ty + 1 + k) % w][(tx + 1 + k) % w] += 2 * d;
            }
        }
    }

    // println!("score={}", score);
    // println!("uy={:?}", uy);

    let mut max_score = score;
    let mut sy = score;

    for dy in 0..w {
        let mut sx = sy;

        for dx in 0..w {
            max_score = max(max_score, sx);

            sx += ux[(dx + 1) % w];
        }

        for dx in 0..w {
            ux[dx] += up[(dy + 1) % w][dx];
        }

        sy += uy[(dy + 1) % w];
    }

    println!("{}", max_score);
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
