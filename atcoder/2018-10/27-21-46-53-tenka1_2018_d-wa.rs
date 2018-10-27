#![allow(unused_imports)]
#![allow(non_snake_case)]

use std::cell::RefCell;
use std::cmp::{max, min, Ordering};
use std::collections::*;
use std::fmt::{Debug, Formatter, Write as FmtWrite};
use std::io::{stderr, stdin, BufRead, Write};
use std::mem::{replace, swap};
use std::ops::*;
use std::rc::Rc;

// -----------------------------------------------
// Framework <https://github.com/vain0x/procon>
// -----------------------------------------------

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
    buf.trim_right().to_owned()
}

// -----------------------------------------------
// Solution
// -----------------------------------------------

fn main() {
    let N = read!(i64);

    let mut k = 1;
    loop {
        let t = k * (k - 1) / 2;
        if t == N {
            let mut G = vec![vec![]; k as usize];
            let mut i = 1_usize;
            for u in 0..k {
                for v in u + 1..k {
                    G[u as usize].push((v as usize, i));
                    G[v as usize].push((u as usize, i));
                    i += 1;
                }
            }

            println!("YES");
            println!("{}", k);
            for u in 0..k {
                let mut xs = G[u as usize]
                    .iter()
                    .map(|&(_, i)| i.to_string())
                    .collect::<Vec<_>>();
                println!("{} {}", G[u as usize].len(), xs.join(" "));
            }
            break;
        } else if t > N {
            println!("NO");
            break;
        } else {
            k += 1;
            continue;
        }
    }
}
