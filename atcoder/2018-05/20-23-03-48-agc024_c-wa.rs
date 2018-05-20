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
        let mut m = 0_i64;
        for i in 0..N {
            if A[i] > m {
                println!("-1");
                return;
            }

            if A[i] == 0 {
                m = 1;
            } else {
                m += 1;
            }
        }
    }

    // seqs[zi] = n: 位置ziのゼロから長さnの列を伸ばしている
    // zi は降順
    let mut seqs = vec![0; N];

    for i in 1..N {
        // assert 0 <= A[i] <= i
        let zi = i - (A[i] as usize);
        seqs[zi] = max(seqs[zi], A[i]);
    }

    let count: i64 = seqs.into_iter().sum();

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
