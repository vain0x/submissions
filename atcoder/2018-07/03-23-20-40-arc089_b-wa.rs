#![allow(unused_imports)]
#![allow(non_snake_case)]

use std::cmp::{max, min};
use std::collections::*;
use std::io::BufRead;
use std::ops::*;
use std::*;

fn solve(K: usize, expects: Vec<(i64, i64, bool)>) -> i32 {
    let K2 = 2 * K;
    let K3 = 3 * K;

    // 解法:
    // 点を同一視して、白四角と黒四角を2つずつ並べた 2K*2K の矩形の上にすべての希望が乗っていることにしていい。また、すべての希望が黒であるとしていい。(白の希望はKずらせばいい。) 模様の種類は 2K*2K 通り。(この矩形の模様の種類と同じだから。)
    //
    // 各希望につき、その希望が叶えられる K*K の範囲が定まる。(範囲は希望に対する相対的な位置のみ考えればいい。以降の計算が平行移動不変だから。) それらの範囲が重なっている個数を各点のスコアと呼ぶ。各点を左上隅の K*K 範囲に集めたとき、そのマスを白四角の左下隅にすれば白マスのスコアが得られる。黒も同様。
    //
    // 希望が矩形の端のほうにあると範囲の「折り返し」が生じて記述がやっかいだが、これは矩形を拡張して 3K*3K にすると解消できる。厚みの計算を高速化するには2次元累積和を使う。

    let mut ds = vec![vec![0_i32; K3 + 1]; K3 + 1];

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

    for y in 0..K3 {
        for x in 0..K3 {
            ds[y + 1][x] += ds[y][x];
        }
    }

    for y in 0..K3 {
        for x in 0..K3 {
            ds[y][x + 1] += ds[y][x];
        }
    }

    let mut T = vec![vec![[0_i32; 2]; K]; K];
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
    ma
}

pub fn main() {
    let (_, k, expects) = {
        let mut line = String::new();
        let stdin = std::io::stdin();
        let mut stdin = stdin.lock();

        stdin.read_line(&mut line).unwrap();
        let words = line.split_whitespace()
            .map(|s| s.parse::<usize>().unwrap())
            .take(2)
            .collect::<Vec<_>>();
        let (N, K) = (words[0], words[1]);

        let mut expects = Vec::new();

        for _ in 0..N {
            stdin.read_line(&mut line).unwrap();
            let words = line.split_whitespace().take(3).collect::<Vec<&str>>();

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
