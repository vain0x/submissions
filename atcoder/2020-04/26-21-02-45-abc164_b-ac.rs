//! Framework <https://github.com/vain0x/procon>

#![allow(non_snake_case)]
#![allow(unused_imports)]

use std::collections::*;

pub struct Scan(Box<dyn Iterator<Item = &'static str>>); // '

impl Scan {
    fn new() -> Self {
        let mut buf = String::new();
        let read_line = move || {
            std::io::stdin().read_line(&mut buf).unwrap();
            Box::leak(buf.split_off(0).into_boxed_str()).split_whitespace()
        };
        Scan(Box::new(std::iter::repeat_with(read_line).flatten()))
    }

    pub fn word<T: std::str::FromStr>(&mut self) -> T {
        self.0.next().unwrap().parse().ok().unwrap()
    }

    pub fn list<T: std::str::FromStr>(&mut self, len: usize) -> Vec<T> {
        std::iter::repeat_with(|| self.word()).take(len).collect()
    }
}

fn main() {
    let mut scan = Scan::new();

    let mut A = scan.word::<i64>();
    let B = scan.word::<i64>();
    let mut C = scan.word::<i64>();
    let D = scan.word::<i64>();

    let takahashi_win;

    loop {
        // takahashi
        C -= B;
        if C <= 0 {
            takahashi_win = true;
            break;
        }

        // aoki
        A -= D;
        if A <= 0 {
            takahashi_win = false;
            break;
        }
    }

    println!("{}", if takahashi_win { "Yes" } else { "No" });
}
