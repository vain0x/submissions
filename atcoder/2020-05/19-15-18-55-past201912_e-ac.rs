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
    let (N, Q) = read!(usize, usize);

    let mut g = vec![BTreeSet::new(); N];
    let mut q = vec![];

    for _ in 0..Q {
        let s = rl();
        let mut s = s.split_whitespace();

        match s.next().unwrap().parse::<i32>().unwrap() {
            1 => {
                let a = s.next().unwrap().parse::<usize>().unwrap() - 1;
                let b = s.next().unwrap().parse::<usize>().unwrap() - 1;
                g[a].insert(b);
            }
            2 => {
                let a = s.next().unwrap().parse::<usize>().unwrap() - 1;
                for u in 0..N {
                    if g[u].contains(&a) {
                        q.push((a, u));
                    }
                }
            }
            3 => {
                let a = s.next().unwrap().parse::<usize>().unwrap() - 1;

                for x in 0..N {
                    if !g[a].contains(&x) {
                        continue;
                    }

                    for y in 0..N {
                        if y == a || !g[x].contains(&y) {
                            continue;
                        }

                        q.push((a, y));
                    }
                }
            }
            _ => unreachable!(),
        }

        while let Some((u, v)) = q.pop() {
            g[u].insert(v);
        }
    }

    let mut row = String::new();
    for u in 0..N {
        row.clear();
        for v in 0..N {
            row += if g[u].contains(&v) { "Y" } else { "N" };
        }
        println!("{}", row);
    }
}
