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

const M: usize = 100_001;

fn main() {
    let mut scan = Scan::new();

    let N = scan.word::<usize>();
    let A = scan.list::<usize>(N);
    let Q = scan.word::<usize>();

    let mut freq = vec![0; M];
    let mut total = 0;
    for &a in &A {
        freq[a] += 1_usize;
        total += a;
    }

    for _ in 0..Q {
        let b = scan.word::<usize>();
        let c = scan.word::<usize>();

        let bn = freq[b];
        freq[c] += bn;
        freq[b] = 0;
        total -= bn * b;
        total += bn * c;
        println!("{}", total);
    }
}

/*

2
1 100000
3
3 4
100000 1
1 100000

*/
