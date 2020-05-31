//! Framework <https://github.com/vain0x/procon>

#![allow(non_snake_case)]
#![allow(unused_imports)]

use std::{collections::*, mem::swap};

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

const P: u32 = 998244353;

fn main() {
    let mut scan = Scan::new();

    let N = scan.word::<usize>();
    let S = scan.word::<u32>();
    let A = scan.list::<u32>(N);

    let mut dp: HashMap<(u32, u32), u32> = HashMap::new();
    let mut dp_next = dp.clone();
    let mut freq = vec![0; N + 1];

    dp.insert((0, 0), 1);

    for i in 0..N {
        dp_next.clear();

        for (&(k, t), &n) in &dp {
            *dp_next.entry((k, t)).or_insert(0) += n;

            if t + A[i] == S {
                freq[k as usize + 1] += n;
            } else if t + A[i] < S {
                *dp_next.entry((k + 1, t + A[i])).or_insert(0) += n;
            }
        }

        swap(&mut dp, &mut dp_next);
    }

    #[cfg(debug_assertions)]
    eprintln!("\x1B[33m{}\x1B[0m = {:?}", "freq", freq);

    let mut total = 0;
    let mut h = 1;
    for k in (1..=N).rev() {
        total += freq[k] * h % P;
        total %= P;

        h *= 2;
        h %= P;
    }

    println!("{}", total);
}
