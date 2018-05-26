#![allow(unused_imports)]
#![allow(non_snake_case)]

use procon::*;
use std::cmp::{max, min};
use std::collections::*;
use std::ops::*;
use std::*;

fn min_heap() -> BinaryHeap<i64> {
    BinaryHeap::new()
}

fn solve(K: usize, Q: usize, A: Vec<i64>) -> i64 {
    let mut score = std::i64::MAX;

    for y in A.iter().cloned() {
        // Y = y になるケースだけ考える。
        // y 未満の数を含む部分列は選択できない。y 未満の数で数列を切断して各部分を島と呼ぶ。
        // 各島について件のルールで最小値を消していくとき、常に列島の最小値は選択できる。とにかく最小値を消していくことでXを最小化できて、ベスト。

        let mut heap = BinaryHeap::new();
        let mut l = 0;
        for r in 0..(A.len() + 1) {
            if r >= A.len() || A[r] < y {
                let r = (if r >= A.len() { r } else { r + 1 });
                let k = r - l;

                // 長さ K 未満の島からはドロップできないので注意
                if k >= K {
                    for j in l..r {
                        heap.push(-A[j]); // 最大値が出てくるヒープなのでマイナスにしておく
                    }
                }
                l = r + 1;
            }
        }

        println!("y={} {:?}", y, heap.iter().cloned().collect::<Vec<_>>());

        // Q 番目に小さい数を探す
        if heap.len() < Q {
            continue;
        }

        let mut x = None;
        for _ in 0..Q {
            x = heap.pop();
        }

        if let Some(x) = x {
            score = min(score, (-x) - y);
        }

        /*
        let mut islands = Vec::new();
        let mut island = min_heap();
        for a in A.iter().cloned() {
            if a < y {
                if island.len() >= K {
                    islands.push(island);
                    island = min_heap();
                }

            // ignore leading <y s
            } else {
                island.push(-a);
            }
        }

        if island.len() >= K {
            islands.push(island);
        }

        println!("y={} {:?}", y, islands);

        // Q 番目に小さい数を探す
        // 長さ K 未満の島からはドロップできないので注意
        let mut heap = min_heap();

        for mut island in islands {
            let c = island.len() + 1 - K;
            for _ in 0..c {
                heap.push(island.pop().unwrap());
            }
        }

        let mut x = None;
        for _ in 0..Q {
            x = heap.pop();
        }

        if let Some(x) = x {
            score = min(score, (-x) - y);
        }
        */
    }
    score
}

pub fn main() {
    let words = read_words::<usize>();
    let (N, K, Q) = (words[0], words[1], words[2]);
    let A = read_words::<i64>();

    println!("{}", solve(K, Q, A));
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

    #[test]
    pub fn test_case1() {
        assert_eq!(1, solve(3, 2, vec![4, 3, 1, 5, 2]));
    }

    #[test]
    pub fn test_case2() {
        assert_eq!(7, solve(1, 6, vec![1, 1, 2, 3, 5, 8, 13, 21, 34, 55]));
    }

    #[test]
    pub fn test_case3() {
        assert_eq!(
            451211184,
            solve(
                7,
                5,
                vec![
                    24979445, 861648772, 623690081, 433933447, 476190629, 262703497, 211047202,
                    971407775, 628894325, 731963982, 822804784,
                ]
            )
        );
    }

}
