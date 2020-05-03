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
    let A = scan.word::<i64>();
    let B = scan.word::<i64>();
    let C = scan.word::<i64>();
    let s = scan.list::<String>(N);

    let mut vars = vec![0; N];
    let mut states = vec![(0, 0, 0); N];
    let mut ok = true;

    {
        for i in 0..N {
            match s[i].as_str() {
                "AB" => {
                    vars[i] = 1;
                }
                "AC" => {
                    vars[i] = 2;
                }
                "BC" => {
                    vars[i] = 2;
                }
                _ => unreachable!(),
            }
        }
    }

    // calc states
    {
        let (mut a, mut b, mut c) = (A, B, C);

        for i in 0..N {
            match (s[i].as_str(), vars[i]) {
                ("AB", 0) => {
                    a += 1;
                    b -= 1;
                }
                ("AB", 1) => {
                    a -= 1;
                    b += 1;
                }
                ("AC", 0) => {
                    a += 1;
                    c -= 1;
                }
                ("AC", 2) => {
                    a -= 1;
                    c += 1;
                }
                ("BC", 1) => {
                    b += 1;
                    c -= 1;
                }
                ("BC", 2) => {
                    b -= 1;
                    c += 1;
                }
                _ => unreachable!(),
            }

            states[i] = (a, b, c);
        }
    }

    // c -> b
    {
        let mut c = states[N - 1].2;
        for i in (0..N).rev() {
            if s[i] == "BC" && c >= 1 {
                vars[i] = 1;
                c -= 1;
                if c == 0 {
                    break;
                }
            }
        }
    }

    // calc states
    {
        let (mut a, mut b, mut c) = (A, B, C);

        for i in 0..N {
            match (s[i].as_str(), vars[i]) {
                ("AB", 0) => {
                    a += 1;
                    b -= 1;
                }
                ("AB", 1) => {
                    a -= 1;
                    b += 1;
                }
                ("AC", 0) => {
                    a += 1;
                    c -= 1;
                }
                ("AC", 2) => {
                    a -= 1;
                    c += 1;
                }
                ("BC", 1) => {
                    b += 1;
                    c -= 1;
                }
                ("BC", 2) => {
                    b -= 1;
                    c += 1;
                }
                _ => unreachable!(),
            }

            states[i] = (a, b, c);
        }
    }

    // c -> a, b -> a
    {
        let mut b = states[N - 1].1;
        let mut c = states[N - 1].2;

        for i in (0..N).rev() {
            if s[i] == "AC" && c >= 1 {
                vars[i] = 0;
                c -= 1;
            }
            if s[i] == "AB" && b >= 1 {
                vars[i] = 0;
                b -= 1;
            }

            if b == 0 && c == 0 {
                break;
            }
        }
    }

    // calc states
    {
        let (mut a, mut b, mut c) = (A, B, C);

        for i in 0..N {
            match (s[i].as_str(), vars[i]) {
                ("AB", 0) => {
                    a += 1;
                    b -= 1;
                }
                ("AB", 1) => {
                    a -= 1;
                    b += 1;
                }
                ("AC", 0) => {
                    a += 1;
                    c -= 1;
                }
                ("AC", 2) => {
                    a -= 1;
                    c += 1;
                }
                ("BC", 1) => {
                    b += 1;
                    c -= 1;
                }
                ("BC", 2) => {
                    b -= 1;
                    c += 1;
                }
                _ => unreachable!(),
            }

            states[i] = (a, b, c);
            ok = a >= 0 && b >= 0 && c >= 0;
        }
    }

    if !ok {
        println!("No");
    } else {
        println!("Yes");
        for i in 0..N {
            println!(
                "{}",
                match vars[i] {
                    0 => "A",
                    1 => "B",
                    2 => "C",
                    _ => unreachable!(),
                }
            );
        }
    }
}
