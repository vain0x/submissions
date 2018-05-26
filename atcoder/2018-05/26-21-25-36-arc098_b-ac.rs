#![allow(unused_imports)]
#![allow(non_snake_case)]

use procon::*;
use std::cmp::{max, min};
use std::collections::*;
use std::ops::*;
use std::*;

pub fn main() {
    let N = read_line().parse::<usize>().unwrap();
    let A = read_words::<i64>();

    let mut l = 0;
    let mut r = 0;
    let mut k = 0;
    let mut bs = 0_i64;

    // [l, l + 1) は数えない

    while l < N {
        // [l, r) はdisjoint
        // [l-i, _) はすべて列挙済み

        while r < N && (bs & A[r]) == 0 {
            bs ^= A[r];
            r += 1;
        }

        // [l, r) は disjoint
        // [l, r - i) はすべて disjoint
        // lが異なる区間はほかのタイミングで数える
        k += r - l;

        // drop leftest
        bs ^= A[l];
        l += 1;
    }

    println!("{}", k);
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
