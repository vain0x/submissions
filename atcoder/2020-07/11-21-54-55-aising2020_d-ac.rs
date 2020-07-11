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

fn solve(X: &str) -> Vec<u64> {
    let N = X.len();
    let pc = X.bytes().filter(|&b| b != b'0').count() as u64;
    if pc == 0 {
        return vec![1; N];
    }

    let bits = X.bytes().map(|b| b - b'0').collect::<Vec<u8>>();
    let x = BigUint::from_radix_be(&bits, 2).unwrap();

    let xp: u64 = (x.clone() % (pc + 1)).to_string().parse::<u64>().unwrap();
    let xm: u64 = if pc > 1 {
        (x.clone() % (pc - 1)).to_string().parse::<u64>().unwrap()
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
    let mut wp = 1_u64;
    let mut wm = 1_u64;

    for (i, &b) in bits.iter().rev().enumerate() {
        let mut y = if b == 0 {
            let m = (pc + 1) as u64;
            (xp + wp) % m
        } else if pc > 1 {
            let m = (pc - 1) as u64;
            (xm + m - wm) % m
        } else {
            ts.push(0);
            wp *= 2;
            wp %= pc + 1;

            if pc > 1 {
                wm *= 2;
                wm %= pc - 1;
            }
            continue;
        };

        #[cfg(debug_assertions)]
        eprintln!("\x1B[33m{}\x1B[0m = {:?}", "(i, y; wp, wm)", (i, y, wp, wm));

        let mut t = 1;
        while y != 0 {
            let pc = u64::count_ones(y) as u64;
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

    scan.word::<usize>();
    let X = scan.word::<String>();

    let ts = solve(&X);
    for t in ts {
        println!("{}", t);
    }
}

/*

3
000
// 1, 1, 1

3
100
// 0, 1, 2

3
001
// 2, 1, 0

*/
