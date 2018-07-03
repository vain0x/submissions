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

fn solve(K: usize, expects: Vec<(i64, i64, bool)>) -> usize {
    let K2 = 2 * K;
    let K3 = 3 * K;

    // 解法:
    // 点を同一視して、白四角と黒四角を2つずつ並べた 2K*2K の矩形の上にすべての希望が乗っていることにしていい。また、すべての希望が黒であるとしていい。(白の希望はKずらせばいい。) 模様の種類は 2K*2K 通り。(この矩形の模様の種類と同じだから。)
    //
    // 各希望につき、その希望が叶えられる K*K の範囲が定まる。(範囲は希望に対する相対的な位置のみ考えればいい。以降の計算が平行移動不変だから。) それらの範囲が重なっている個数を各点のスコアと呼ぶ。各点を左上隅の K*K 範囲に集めたとき、そのマスを白四角の左下隅にすれば白マスのスコアが得られる。黒も同様。
    //
    // 希望が矩形の端のほうにあると範囲の「折り返し」が生じて記述がやっかいだが、これは矩形を拡張して 3K*3K にすると解消できる。厚みの計算を高速化するには2次元累積和を使う。

    let mut ds = vec![vec![0_i64; K3 + 1]; K3 + 1];

    for (x, y, black) in expects {
        // (y, x): black <=> (y + 2K Z, x + 2K Z): black
        let y = (y % K2 as i64) as usize;
        let x = (x % K2 as i64) as usize;

        // (y, x): white <=> (y + K, x): black
        let y = if black { y } else { y + K };

        ds[y][x] += 1;
        ds[y + K][x] -= 1;
        ds[y][x + K] -= 1;
        ds[y + K][x + K] += 1;
    }

    debug!("ds={:?}", ds);

    for y in 0..K2 {
        for x in 0..K2 + 1 {
            ds[y + 1][x] += ds[y][x];
        }
    }

    debug!("ds={:?}", ds);

    for y in 0..K2 + 1 {
        for x in 0..K2 {
            ds[y][x + 1] += ds[y][x];
        }
    }

    debug!("ds={:?}", ds);

    let mut T = vec![vec![[0_i64; 2]; K + 1]; K + 1];
    for y in 0..K3 {
        for x in 0..K3 {
            let black = (y / K + x / K) % 2;
            T[y % K][x % K][black] += ds[y][x];
        }
    }

    let mut ma = 0;
    for black in 0..2 {
        for y in 0..K {
            for x in 0..K {
                ma = max(ma, T[y][x][black]);
            }
        }
    }
    ma as usize
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
