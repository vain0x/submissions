#![allow(non_snake_case)]

use std::io::*;

#[allow(dead_code)]
fn rl() -> String {
    let mut buf = String::new();
    stdin().read_line(&mut buf).unwrap();
    buf.trim_right().to_owned()
}

pub fn main() {
    let line = rl();
    let mut A = line
        .trim_right()
        .split_whitespace()
        .map(|x| x.parse().unwrap())
        .collect::<Vec<i32>>();
    A.sort();
    println!("{}", A.last().unwrap() - A[0]);
}
