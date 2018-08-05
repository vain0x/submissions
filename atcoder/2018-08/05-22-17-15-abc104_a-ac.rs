#![allow(non_snake_case)]

use std::io::*;

#[allow(dead_code)]
fn rl() -> String {
    let mut buf = String::new();
    stdin().read_line(&mut buf).unwrap();
    buf.trim_right().to_owned()
}

pub fn main() {
    let R = rl().parse::<i32>().unwrap();
    if R < 1200 {
        println!("ABC")
    } else if R < 2800 {
        println!("ARC")
    } else {
        println!("AGC")
    }
}
