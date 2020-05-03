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

fn choose(
    A: &mut i64,
    B: &mut i64,
    a: i64,
    b: i64,
    a_name: &'static str,
    b_name: &'static str,
) -> Option<&'static str> {
    if *A == 0 && *B == 0 {
        return None;
    }

    if *A >= a {
        *A -= 1;
        *B += 1;
        return Some(b_name);
    }

    if *B >= b {
        *A += 1;
        *B -= 1;
        return Some(a_name);
    }

    if (A.saturating_sub(a), *A) >= (B.saturating_sub(b), *B) {
        *A -= 1;
        *B += 1;
        Some(b_name)
    } else {
        *A += 1;
        *B -= 1;
        Some(a_name)
    }
}

fn main() {
    let mut scan = Scan::new();

    let N = scan.word::<usize>();
    let mut A = scan.word::<i64>();
    let mut B = scan.word::<i64>();
    let mut C = scan.word::<i64>();
    let s = scan.list::<String>(N);

    let mut ops = vec![(0, 0, 0); N];

    for i in (0..N).rev() {
        let (mut a, mut b, mut c) = if i == N - 1 { (0, 0, 0) } else { ops[i + 1] };
        match s[i].as_str() {
            "AB" => {
                a += 1;
                b += 1;
            }
            "AC" => {
                a += 1;
                c += 1;
            }
            "BC" => {
                b += 1;
                c += 1;
            }
            _ => unreachable!(),
        }

        ops[i] = (a, b, c);
    }

    let mut vars = vec![];
    let mut ok = true;

    for i in 0..N {
        match s[i].as_str() {
            "AB" => match choose(&mut A, &mut B, ops[i].0, ops[i].1, "A", "B") {
                Some(name) => vars.push(name),
                None => {
                    ok = false;
                    break;
                }
            },
            "AC" => match choose(&mut A, &mut C, ops[i].0, ops[i].2, "A", "C") {
                Some(name) => vars.push(name),
                None => {
                    ok = false;
                    break;
                }
            },
            "BC" => match choose(&mut C, &mut B, ops[i].2, ops[i].1, "C", "B") {
                Some(name) => vars.push(name),
                None => {
                    ok = false;
                    break;
                }
            },
            _ => unreachable!(),
        }
    }

    if !ok {
        println!("No");
    } else {
        println!("Yes");
        for i in 0..N {
            println!("{}", vars[i]);
        }
    }
}
