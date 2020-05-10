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

fn calc(s: &str) -> (i64, i64) {
    let mut balance = 0;
    let mut least = 0;

    for c in s.chars() {
        match c {
            '(' => {
                balance += 1;
            }
            ')' => {
                balance -= 1;
                least = least.min(balance);
            }
            _ => unreachable!(),
        }
    }

    ((-least).max(0), balance)
}

fn main() {
    let mut scan = Scan::new();

    let N = scan.word::<usize>();
    let S = scan.list::<String>(N);

    let mut T = BTreeMap::new();

    for s in S {
        let (r, b) = calc(&s);
        T.entry(r).or_insert(vec![]).push(b);
    }

    for (_, v) in &mut T {
        v.sort();
    }

    let mut ok = true;
    let mut balance = 0_i64;

    use std::cmp::Reverse;
    use std::ops::Bound::*;

    for _ in 0..N {
        match T.range_mut(balance..).next() {
            Some((&least, diffs)) => {
                if balance < least {
                    ok = false;
                    break;
                }

                balance += diffs.pop().unwrap();
                if balance < 0 {
                    ok = false;
                    break;
                }

                if diffs.is_empty() {
                    T.remove(&least);
                }
            }
            None => {
                ok = false;
                break;
            }
        }
    }

    println!("{}", if ok { "Yes" } else { "No" });
}
