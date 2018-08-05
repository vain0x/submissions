#![allow(non_snake_case)]

use std::io::*;

#[allow(dead_code)]
fn rl() -> String {
    let mut buf = String::new();
    stdin().read_line(&mut buf).unwrap();
    buf.trim_right().to_owned()
}

pub fn main() {
    let S = rl().chars().collect::<Vec<char>>();

    let ok = S[0] == 'A'
        && S.last().unwrap().is_lowercase()
        && S[2..S.len() - 1].iter().filter(|&&c| c == 'C').count() == 1
        && S[1..].iter().filter(|&&c| !c.is_lowercase()).count() == 1;
    println!("{}", if ok { "AC" } else { "WA" })
}
