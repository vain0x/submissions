#![allow(unused_imports)]
#![allow(non_snake_case)]

use std::*;
use std::collections::*;
use std::ops::*;
use procon::*;

pub fn main() -> () {
    let _ = read_line();
    println!(
        "{}",
        if read_line().contains('Y') {
            "Four"
        } else {
            "Three"
        }
    );
    return;
}

pub mod procon {
    use std;
    use std::io;
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

    pub fn read_vec<T>(len: usize) -> Vec<T>
    where
        T: std::str::FromStr,
        T::Err: std::fmt::Debug,
    {
        let mut vec = Vec::new();
        while vec.len() < len {
            let words = read_words();
            for word in words {
                vec.push(word);
            }
        }
        vec
    }

    pub fn make_vec<T, F>(len: usize, f: F) -> Vec<T>
    where
        F: Fn(usize) -> T,
    {
        return (0..len).into_iter().map(f).collect();
    }
}
