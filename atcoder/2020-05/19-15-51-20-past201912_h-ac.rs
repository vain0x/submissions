//! Framework <https://github.com/vain0x/procon>

#![allow(non_snake_case)]
#![allow(unused_imports)]

use std::cmp::{max, min, Ordering};
use std::collections::*;
use std::fmt::{Debug, Display, Formatter, Write as FmtWrite};
use std::io::{stderr, stdin, BufRead, Write};

#[allow(unused_macros)]
macro_rules! debug {
    ($($e:expr),*) => {
        #[cfg(debug_assertions)]
        $({
            let (e, mut err) = (stringify!($e), stderr());
            writeln!(err, "\x1B[33m{}\x1B[0m = {:?}", e, $e).unwrap()
        })*
    };
}

#[allow(unused_macros)]
macro_rules! read {
    ([$t:ty] ; $n:expr) =>
        ((0..$n).map(|_| read!([$t])).collect::<Vec<_>>());
    ($($t:ty),+ ; $n:expr) =>
        ((0..$n).map(|_| read!($($t),+)).collect::<Vec<_>>());
    ([$t:ty]) =>
        (rl().split_whitespace().map(|w| w.parse().unwrap()).collect::<Vec<$t>>());
    ($($t:ty),*) => {{
        let buf = rl();
        let mut w = buf.split_whitespace();
        ($(w.next().unwrap().parse::<$t>().unwrap()),*)
    }};
}

#[allow(dead_code)]
fn rl() -> String {
    let mut buf = String::new();
    stdin().read_line(&mut buf).unwrap();

    #[allow(deprecated)]
    buf.trim_right().to_owned()
}

// -----------------------------------------------
// Solution
// -----------------------------------------------

fn main() {
    let N = read!(usize);
    let mut C = read![[usize]];
    let Q = read!(usize);

    let mut unit_sell = 0;
    let mut even_sell = 0;
    let mut odd_sell = 0;

    let mut even_count = 0;
    let mut even_min = std::usize::MAX;
    let mut odd_count = 0;
    let mut odd_min = std::usize::MAX;

    for i in 0..N {
        if i % 2 == 0 {
            even_count += 1;
            even_min = min(even_min, C[i]);
        } else {
            odd_count += 1;
            odd_min = min(odd_min, C[i]);
        }
    }

    for _ in 0..Q {
        let S = rl();
        let mut w = S.split_whitespace();
        match w.next().unwrap().parse::<i32>().unwrap() {
            1 => {
                let x = w.next().unwrap().parse::<usize>().unwrap() - 1;
                let a = w.next().unwrap().parse::<usize>().unwrap();

                let mut current = C[x];
                if x % 2 == 0 {
                    current = current.saturating_sub(even_sell);
                } else {
                    current = current.saturating_sub(odd_sell);
                }
                if current < a {
                    continue;
                }

                unit_sell += a;
                C[x] -= a;

                if x % 2 == 0 {
                    even_min = min(even_min, C[x]);
                } else {
                    odd_min = min(odd_min, C[x]);
                }
            }
            2 => {
                let a = w.next().unwrap().parse::<usize>().unwrap();

                let current = even_min.saturating_sub(even_sell);
                if current < a {
                    continue;
                }

                even_sell += a;
            }
            3 => {
                let a = w.next().unwrap().parse::<usize>().unwrap();

                let current = min(
                    even_min.saturating_sub(even_sell),
                    odd_min.saturating_sub(odd_sell),
                );
                if current < a {
                    continue;
                }

                even_sell += a;
                odd_sell += a;
            }
            _ => unreachable!(),
        }
    }

    let total = unit_sell + even_count * even_sell + odd_count * odd_sell;
    println!("{}", total)
}
