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

    let N = scan.word::<usize>();
    let A = scan.list::<usize>(N + 1);

    let mut ok = true;
    let mut total = 0;

    if (0..=N).any(|d| A[d] > (1 << d)) {
        ok = false;
    } else {
        let mut b = vec![0; N + 1];
        for d in (0..N).rev() {
            let ub = (A[d + 1] + b[d + 1]).min((1 << d) - A[d]);
            b[d] = ub;
        }

        #[cfg(debug_assertions)]
        eprintln!("\x1B[33m{}\x1B[0m = {:?}", "b", b);

        for d in 0..N {
            b[d + 1] = b[d + 1].min((b[d] * 2).saturating_sub(A[d + 1]));
        }

        #[cfg(debug_assertions)]
        eprintln!("\x1B[33m{}\x1B[0m = {:?}", "b", b);

        ok = ok && (0..N).all(|d| b[d] >= (A[d + 1] + b[d + 1] + 1) / 2);
        total = (0..=N).map(|d| A[d] + b[d]).sum::<usize>();
    }

    if ok {
        println!("{}", total);
    } else {
        println!("-1");
    }
}
