#![allow(unused_imports)]
#![allow(non_snake_case)]

use procon::*;
use std::cmp::{max, min};
use std::collections::*;
use std::ops::*;
use std::*;

pub fn main() {
    let words = read_words::<i32>();
    let (A, B, C, X, Y) = (
        words[0],
        words[1],
        words[2],
        words[3] as i32,
        words[4] as i32,
    );

    let total = (0..max(X, Y) + 1)
        .map(|N| {
            // ABピザを 2N 枚買う
            C * (2 * N) + A * max(0, X - N) + B * max(0, Y - N)
        })
        .min()
        .unwrap();

    println!("{}", total);
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

    pub trait IteratorExt: Iterator {
        fn vec(self) -> Vec<Self::Item>
        where
            Self: Sized,
        {
            self.collect()
        }
    }

    impl<T: Iterator> IteratorExt for T {}
}
