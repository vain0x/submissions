#![allow(unused_imports)]
#![allow(non_snake_case)]

use procon::*;
use std::cmp::{max, min};
use std::collections::*;
use std::ops::*;
use std::*;

pub fn main() {
    let N = read_line().parse::<usize>().unwrap();
    let S = read_line().chars().collect::<Vec<char>>();

    let mut ek = vec![0; N];
    let mut wk = vec![0; N];
    for i in 0..N {
        wk[i] = (if i >= 1 { wk[i - 1] } else { 0 }) + (if S[i] == 'W' { 1 } else { 0 });
    }

    for i in (0..N).rev() {
        ek[i] = (if i + 1 < N { ek[i + 1] } else { 0 }) + (if S[i] == 'E' { 1 } else { 0 });
    }

    let mut m = N;
    for i in 0..N {
        let l = if i >= 1 { wk[i - 1] } else { 0 };
        let r = if i + 1 < N { ek[i + 1] } else { 0 };
        m = min(m, l + r);
    }

    println!("{}", m);

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
