#![allow(unused_imports)]
#![allow(non_snake_case)]

use procon::*;
use std::cmp::{max, min};
use std::collections::*;
use std::ops::*;
use std::*;

macro_rules! debug {
    ($format:expr, $($args:expr),*) => {
        print_debug(format!($format, $($args),*))
    };
}

pub fn print_debug(_content: String) {
    // println!("{}", content);
}

fn solve(k: usize, expects: Vec<(i64, i64, bool)>) -> usize {
    let w = k * 2;
    let W = w as i64;

    let wrap = |x: usize| if x == w { w } else { x % w };

    // 縦と横に累積和をとると S になる
    // S[x][y] = 黒四角の左下隅が (x, y) であるときのスコア
    let mut ds = vec![vec![0_i32; w + 1]; w + 1];

    for (x, y, black) in expects {
        // (x, y): black <=> (x + W*Z, y + W*Z): black
        let x = ((x % W + W) % W) as usize;
        let y = ((y % W + W) % W) as usize;

        // (x, y): black <=> (x + k, y): white
        let x = if black { x } else { (x + k) % w };

        let vx = (w + x + 1 - k) % w;
        let vy = (w + y + 1 - k) % w;

        let ux = (x + 1) % w;
        let uy = (y + 1) % w;

        let areas = &[
            ((vx, vy), (wrap(vx + k), wrap(vy + k))),
            ((ux, uy), (wrap(ux + k), wrap(uy + k))),
        ];

        // S の (xl, yl)..(xr, yr) の範囲に各々 1 を足したい
        // 素朴にやると O(K^2) になってしまうので、ここでは差分だけ足しておいて、あとで2次元累積和をとる。
        // 注意 1. 範囲の端が w*w 矩形の端に一致するときは足しっぱなしでいい
        // 注意 2. 開区間で表しているのでxr,yr は範囲外にあることがある ((w,w)など)
        // 注意 3. 範囲が端と重なっている場合は折り返されて2つの領域になる。

        let split = |((xl, yl), (xr, yr))| {
            if xl > xr && yl > yr {
                vec![((0, 0), (xr, yr)), ((xl, yl), (w, w))]
            } else if xl > xr && yl <= yr {
                vec![((xl, yl), (w, yr)), ((0, yl), (xr, yr))]
            } else if xl <= xr && yl > yr {
                vec![((xl, yl), (xr, w)), ((xl, 0), (xr, yr))]
            } else {
                // xl <= xr, yl <= yr
                vec![((xl, yl), (xr, yr))]
            }
        };

        for &area in areas {
            for ((xl, yl), (xr, yr)) in split(area) {
                ds[xl][yl] += 1;
                ds[xl][yr] -= 1;
                ds[xr][yl] -= 1;
                ds[xr][yr] += 1;
            }
        }

        debug!("({}, {})->", x, y);
        for &area in areas {
            for ((xl, yl), (xr, yr)) in split(area) {
                debug!("  ({}, {})..({}, {})", xl, yl, xr, yr);
            }
        }
    }

    debug!("ds={:?}", ds);

    for x in 0..w {
        for y in 1..w {
            ds[x][y] += ds[x][y - 1];
        }
    }

    debug!("ds={:?}", ds);

    for y in 0..w {
        for x in 1..w {
            ds[x][y] += ds[x - 1][y];
        }
    }

    debug!("ds={:?}", ds);

    let mut max_score = 0;
    for y in 0..w {
        for x in 0..w {
            max_score = max(max_score, ds[x][y]);
        }
    }
    max_score as usize
}

pub fn main() {
    let (_, k, expects) = {
        let words = read_words::<usize>();
        let (N, K) = (words[0], words[1]);

        let mut expects = Vec::new();

        for _ in 0..N {
            let words = read_words::<String>();

            let (x, y, black) = (
                words[0].parse::<i64>().unwrap(),
                words[1].parse::<i64>().unwrap(),
                words[2] == "B",
            );

            expects.push((x, y, black));
        }
        (N, K, expects)
    };

    println!("{}", solve(k, expects));
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

#[cfg(test)]
pub mod tests {
    use super::*;

    const W: bool = false;
    const B: bool = true;

    #[test]
    fn test_case1() {
        let expects = vec![(0, 1, W), (1, 2, W), (5, 3, B), (5, 4, B)];
        assert_eq!(4, solve(3, expects));
    }

    #[test]
    fn test_case2() {
        let expects = vec![
            (1, 2, B),
            (2, 1, W),
            (2, 2, B),
            (1, 0, B),
            (0, 6, W),
            (4, 5, W),
        ];
        assert_eq!(4, solve(2, expects));
    }
}
