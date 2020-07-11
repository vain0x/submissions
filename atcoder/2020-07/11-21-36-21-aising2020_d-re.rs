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

use num_bigint::BigUint;
use std::mem::replace;

fn solve(X: &str) -> Vec<usize> {
    let N = X.len();

    let bits = X.bytes().map(|b| b - b'0').collect::<Vec<u8>>();
    let pc = X.bytes().filter(|&b| b != b'0').count();
    let x = BigUint::from_radix_be(&bits, 2).unwrap();

    let xp = (x.clone() % (pc + 1)).to_string().parse::<usize>().unwrap();
    let xm = if pc > 1 {
        (x.clone() % (pc - 1)).to_string().parse::<usize>().unwrap()
    } else {
        0
    };

    #[cfg(debug_assertions)]
    eprintln!(
        "\x1B[33m{}\x1B[0m = {:?}",
        "(&bits, pc, &x, &xp, &xm)",
        (&bits, pc, &x, &xp, &xm)
    );

    let mut ts = vec![];
    let mut wp = 1_usize;
    let mut wm = 1_usize;

    for i in 0..N {
        let mut y = if bits[N - 1 - i] == 0 {
            let m = pc + 1;
            (xp + wp) % m
        } else {
            let m = pc - 1;
            (xm + m - wm) % m
        };

        #[cfg(debug_assertions)]
        eprintln!("\x1B[33m{}\x1B[0m = {:?}", "(i, y)", (i, y));

        let mut t = 1;
        while y != 0 {
            let pc = usize::count_ones(y) as usize;
            y %= pc;
            t += 1;
        }

        ts.push(t);

        wp *= 2;
        wp %= pc + 1;

        if pc > 1 {
            wm *= 2;
            wm %= pc - 1;
        }
    }

    ts.reverse();
    ts
}

fn main() {
    let mut scan = Scan::new();

    let N = scan.word::<usize>();
    let X = scan.word::<String>();

    let ts = if X.bytes().all(|b| b == b'0') {
        vec![0; N]
    } else {
        solve(&X)
    };

    for t in ts {
        println!("{}", t);
    }
}
