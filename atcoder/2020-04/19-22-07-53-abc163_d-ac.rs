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

const P: i64 = 1_000_000_007;

fn main() {
    let mut scan = Scan::new();

    let N = scan.word::<usize>();
    let K = scan.word::<usize>();

    // u[i] = (i+1 個の整数の最小の和)
    let mut u = vec![0];
    for i in 1..N + 1 {
        u.push(u.last().unwrap().clone() + i);
    }

    // v[i] = (i+1 個の整数の最大の和)
    let mut v = vec![N];
    for i in (0..N).rev() {
        v.push(v.last().unwrap().clone() + i);
    }

    let mut total = 0_i64;

    for k in K..N + 2 {
        if u[k - 1] <= v[k - 1] + 1 {
            total += (v[k - 1] + 1 - u[k - 1]) as i64;
            total %= P;
        }
    }

    println!("{}", total);
}
