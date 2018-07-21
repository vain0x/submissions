#![allow(non_snake_case)]

use std::io::*;

#[allow(dead_code)]
fn rl() -> String {
    let mut buf = String::new();
    stdin().read_line(&mut buf).unwrap();
    buf.trim_right().to_owned()
}

pub fn main() {
    let S = rl();
    let T = rl();
    println!(
        "{}",
        if format!("{}{}", S, S).contains(&T) {
            "Yes"
        } else {
            "No"
        }
    );
}
