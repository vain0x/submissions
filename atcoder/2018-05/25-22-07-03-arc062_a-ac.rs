#![allow(unused_imports)]
#![allow(non_snake_case)]

use procon::*;
use std::cmp::{max, min};
use std::collections::*;
use std::ops::*;
use std::*;

pub fn main() {
    let N = read_line().parse::<usize>().unwrap();
    let mut T = Vec::new();
    let mut A = Vec::new();

    for _ in 0..N {
        let words = read_words::<i64>();
        T.push(words[0]);
        A.push(words[1]);
    }

    let mut cur = (T[0], A[0]);
    for i in 1..N {
        let (t, a) = cur;

        // cur' = (k T[i], k A[i])

        // min k s.t.
        // k T[i] >= t
        // k A[i] >= a

        let k = max((t + T[i] - 1) / T[i], (a + A[i] - 1) / A[i]);

        cur = (k * T[i], k * A[i]);
    }

    println!("{}", cur.0 + cur.1);
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
