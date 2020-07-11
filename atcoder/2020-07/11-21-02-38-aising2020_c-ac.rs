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

    for n in 1..=N {
        let mut count = 0_usize;
        let mut x = 1;
        while x * x <= n {
            let x2 = x * x;

            let mut y = 1;
            while x2 + y * y <= n {
                let y2 = y * y;

                let mut z = 1;
                while x2 + y2 + z * z <= n {
                    let z2 = z * z;

                    if x2 + y2 + z2 + x * y + y * z + z * x == n {
                        count += 1;
                    }

                    z += 1;
                }

                y += 1;
            }

            x += 1;
        }

        println!("{}", count);
    }
}
