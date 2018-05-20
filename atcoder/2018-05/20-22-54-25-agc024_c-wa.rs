#![allow(unused_imports)]
#![allow(non_snake_case)]

use procon::*;
use std::cmp::{max, min};
use std::collections::*;
use std::ops::*;
use std::*;

pub fn main() {
    let N = read_line().parse::<usize>().unwrap();
    let A = (0..N)
        .map(|_| read_line().parse::<i64>().unwrap())
        .collect::<Vec<_>>();

    {
        // 最後にゼロを見た位置
        let mut j = 0;
        for i in 0..N {
            if A[i] + (j as i64) > (i as i64) {
                println!("-1");
                return;
            }
            if A[i] == 0 {
                j = i;
            }
        }
    }

    // seqs[_] = (zi, n): 配列Aの位置ziにある0から長さnの列を伸ばしている
    // zi は降順
    let mut seqs = (0..N).map(|i| (i as i64, 0)).collect::<Vec<_>>();

    for i in 1..N {
        // assert 0 <= A[i] <= i

        // シーケンスのインデックス
        let si = {
            if seqs.is_empty() {
                None
            } else {
                let mut l = 0; // ok
                let mut r = seqs.len(); // ng

                while r - l >= 2 {
                    let m = l + (r - l) / 2;
                    let (zi, _) = seqs[m];

                    // この数列を位置iまで伸ばしたときiにくる値
                    let cur = (i as i64) - zi;

                    if cur >= A[i] {
                        l = m;
                    } else {
                        r = m;
                    }
                }

                let (zi, _) = seqs[l];
                if ((i as i64) - zi) == A[i] {
                    Some(l)
                } else {
                    None
                }
            }
        };

        match si {
            Some(si) => {
                let (zi, _) = seqs[si];
                seqs[si] = (zi, (i as i64) - zi);
            }
            None => {
                seqs.push(((i as i64) - A[i], A[i]));
            }
        }
    }

    let count: i64 = seqs.into_iter().map(|(_, n)| n).sum();

    println!("{}", count);
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
