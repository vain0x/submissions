#![allow(unused_imports)]
#![allow(non_snake_case)]

use procon::*;
use std::cmp::{max, min};
use std::collections::*;
use std::ops::*;
use std::*;

pub fn main() {
    let S = read_line().chars().rev().collect::<Vec<char>>();

    let words = vec!["dream", "dreamer", "erase", "eraser"]
        .into_iter()
        .map(|s| s.chars().rev().collect::<Vec<char>>())
        .collect::<Vec<_>>();

    let mut i = 0;

    let mut ok = true;
    loop {
        if i >= S.len() {
            break;
        }

        let next = words
            .iter()
            .filter_map(|word| {
                if S[i..].starts_with(word) {
                    Some(i + word.len())
                } else {
                    None
                }
            })
            .next();

        if let Some(j) = next {
            i = j;
            continue;
        } else {
            ok = false;
            break;
        }
    }

    println!("{}", if ok { "YES" } else { "NO" });

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
