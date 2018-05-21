#![allow(unused_imports)]
#![allow(non_snake_case)]

use procon::*;
use std::cmp::{max, min};
use std::collections::*;
use std::ops::*;
use std::*;

type Point = (usize, usize);

#[derive(PartialEq, Clone, Copy, Debug)]
enum Color {
    B,
    W,
}

// ある黒四角の左下隅が origin であるような塗り
struct Problem<'a> {
    k: usize,
    w: usize,
    expects: &'a Vec<(Point, Color)>,
    origin: Point,
    //score: i32,
}

impl<'a> Problem<'a> {
    fn color(&self, (y, x): Point) -> Color {
        if (y < self.k) ^ (x < self.k) {
            Color::W
        } else {
            Color::B
        }
    }

    pub fn score(&self) -> usize {
        let (dy, dx) = self.origin;
        self.expects
            .iter()
            .filter(|&&((y, x), c)| {
                let y1 = (self.w + y - dy) % self.w;
                let x1 = (self.w + x - dx) % self.w;
                self.color((y1, x1)) == c
            })
            .count()
    }

    // diff[i] = 黒四角を右に i 動かしたときのスコアの増減
    pub fn calc(&self) -> Vec<i32> {
        let (dy, dx) = self.origin;

        //println!("O={:?} score={:?}", (dy, dx), self.score());

        let mut diff = vec![0_i32; self.w];
        for ((y, x), c) in self.expects.iter().cloned() {
            let y1 = (self.w + y - dy) % self.w;
            let x1 = (self.w + x - dx) % self.w;
            let current = self.color((y1, x1));
            let ok = current == c;

            let t = x1 % self.k;

            //println!("({}, {}, {:?}, {}, {})", y1, x1, current, ok, t);

            // t+1 シフトすると色が変わる
            //println!("diff[{}] += {}", (t + 1) % self.w, if ok { -1 } else { 1 });
            diff[(t + 1) % self.w] += if ok { -1 } else { 1 };

            // さらにkシフトするともとの色になる
            /*println!(
                "diff[{}] += {}",
                (t + 1 + self.k) % self.w,
                if ok { 1 } else { -1 }
            );*/
            diff[(t + 1 + self.k) % self.w] += if ok { 1 } else { -1 };
        }

        //println!("O={:?} diff={:?}", (dy, dx), diff);

        diff
    }
}

pub fn main() {
    let (_, K, w, expects) = {
        let words = read_words::<usize>();
        let (N, K) = (words[0], words[1]);
        let w = K * 2;
        let mut expects = Vec::new();
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

            expects.push(((y, x), c));
        }
        (N, K, w, expects)
    };

    let transposed = expects
        .iter()
        .map(|&((y, x), c)| ((w - x, y), c))
        .collect::<Vec<_>>();

    // 一辺 K の黒い四角を (0, 0) が左下隅になるように配置して、市松模様を塗る。
    // これを右または上に1つずつ動かしていくとき、色が変化したマスの個数を数えることでスコアを計算する。

    let py = Problem {
        origin: (0, 0),
        k: K,
        w: w,
        expects: &transposed,
        //score: 0,
    };

    let vy = py.calc();

    let vxs = (0..w)
        .map(|dy| {
            let px = Problem {
                origin: (dy, 0),
                k: K,
                w: w,
                expects: &expects,
                //score: 0,
            };
            px.calc()
        })
        .collect::<Vec<_>>();

    let mut max_score = 0;
    let mut sy = py.score() as i32;

    for dy in 0..K {
        let mut sx = sy;

        for dx in 0..K {
            max_score = max(max_score, sx);

            sx += vxs[dy][(dx + 1) % w];
        }

        sy += vy[(dy + 1) % w];
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
	
    	