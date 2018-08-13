#![allow(non_snake_case)]

use std::io::*;

pub fn main() {
    let mut buf = String::new();
    stdin().read_line(&mut buf).unwrap();
    let xs = buf
        .trim_right()
        .split_whitespace()
        .map(|s| s.parse().unwrap())
        .collect::<Vec<i32>>();
    let N = xs[0];
    let K = xs[1];
    let r = if N % K == 0 { 0 } else { 1 };
    println!("{}", r);
}
