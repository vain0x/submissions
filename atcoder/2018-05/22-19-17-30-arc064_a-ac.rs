#![allow(unused_imports)]
#![allow(non_snake_case)]

use procon::*;
use std::cmp::{max, min};
use std::collections::*;
use std::ops::*;
use std::*;

pub fn main() {
    let (N, x) = {
        let words = read_words::<usize>();
        (words[0], words[1] as i64)
    };
    let mut A = read_words::<i64>();

    let mut count = 0_i64;
    for i in 1..N {
        let y = A[i] + A[i - 1];

        // 食べる個数
        let z = max(0, y - x);

        // i 番目から食べる個数
        let az = min(A[i], z);

        // (i - 1) 番目から食べる個数
        let bz = z - az;

        A[i] -= az;
        A[i - 1] -= bz;

        count += z;
    }
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
