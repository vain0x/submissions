#![allow(unused_imports)]
#![allow(non_snake_case)]

use std::cmp::{max, min, Ordering};
use std::collections::*;
use std::io::*;
use std::ops::*;
use std::*;

// -----------------------------------------------
// Framework
// -----------------------------------------------

#[allow(unused)]
fn rl() -> String {
    let mut buf = String::new();
    io::stdin().read_line(&mut buf).unwrap();
    buf.trim_right().to_owned()
}

// -----------------------------------------------
// Solution
// -----------------------------------------------

pub fn main() {
    let N = rl().parse::<i64>().unwrap();

    let mut m = 1_i64;
    let mut s = 0_i64; // sum of 1..m
    while s + m < N {
        s += m;
        m += 1;
    }

    // このとき sum(1..m) < N <= sum(1..m + 1) となる。
    assert!(s < N && s + m >= N);

    let d = s + m - N;
    assert!(0 <= d && d <= N);

    // 1 + 2 + ... + m - d = N なので、解くべき集合は:

    for x in 1..m + 1 {
        if x == d {
            continue;
        }

        println!("{}", x);
    }
    return;
}
