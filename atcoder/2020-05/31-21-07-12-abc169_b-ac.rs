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
    let A = scan.list::<u64>(N);

    let mut ok = true;
    let mut prod = 1_u64;
    if (0..N).any(|i| A[i] == 0) {
        prod = 0;
    } else {
        for i in 0..N {
            prod = match prod.overflowing_mul(A[i]) {
                (_, true) => {
                    ok = false;
                    break;
                }
                (prod, false) => {
                    if prod > 1_000_000_000_000_000_000 {
                        ok = false;
                        break;
                    }

                    prod
                }
            };
        }
    }

    if ok {
        println!("{}", prod);
    } else {
        println!("-1");
    }
}
