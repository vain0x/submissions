#![allow(unused_imports)]
#![allow(non_snake_case)]

use std::*;
use std::collections::*;
use std::ops::*;
use procon::*;

pub fn main() -> () {
    let (H, W, D) = {
        let xs = read_words::<usize>();
        (xs[0], xs[1], xs[2])
    };
    let mut A = make_vec(H, |_| make_vec(W, |_| 0));
    for y in 0..H {
        for (x, a) in read_words::<usize>().into_iter().enumerate() {
            A[y][x] = a - 1;
        }
    }

    let costs = {
        let HW = H * W;

        let distance =
            |(y1, x1): (i32, i32), (y2, x2): (i32, i32)| (y1 - y2).abs() + (x2 - x1).abs();

        let mut points = make_vec(HW, |_| (0, 0));
        for y in 0..H {
            for x in 0..W {
                points[A[y][x]] = (y as i32, x as i32);
            }
        }

        let mut costs = make_vec(HW, |_| 0i64);
        for i in 0..HW {
            costs[i] = if i < D {
                0
            } else {
                let d = distance(points[i], points[i - D]) as i64;
                costs[i - D] + d
            };
        }

        costs
    };

    let Q = read_line().parse::<usize>().unwrap();
    for _ in 0..Q {
        let (l, r) = {
            let xs = read_words::<usize>();
            (xs[0] - 1, xs[1] - 1)
        };
        let cost = costs[r] - costs[l];
        println!("{}", cost);
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

    pub fn make_vec<T, F>(len: usize, f: F) -> Vec<T>
    where
        F: Fn(usize) -> T,
    {
        return (0..len).into_iter().map(f).collect();
    }
}
