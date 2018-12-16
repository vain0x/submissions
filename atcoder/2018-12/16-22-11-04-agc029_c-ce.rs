// -----------------------------------------------
// Framework <https://github.com/vain0x/procon>
//
// See the bottom of file for solution.
// -----------------------------------------------

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
    read!(usize);
    let A = read![[i64]];

    let mut L = 0; // ng
    let mut R = A.len() + 1; // ok

    while (R - L) > 1 {
        let m = (L + R) / 2;
        let mut ok = true;

        let mut f = BTreeMap::new();

        'fin: for i in 0..A.len() {
            let prev = if i == 0 { 0 } else { A[i - 1] };

            if A[i] > prev {
                continue;
            }

            for (_, a) in f.range_mut(A[i]..) {
                *a = 0;
            }
            debug!((i, prev, A[i], &f));

            let mut cur = A[i] - 1;
            loop {
                {
                    let e = f.entry(cur).or_insert(0);
                    *e += 1;
                    if *e < m {
                        break;
                    }
                }

                if cur == 0 {
                    ok = false;
                    break 'fin;
                }

                f.remove(&cur);
                cur -= 1;
            }
        }

        if ok {
            R = m;
        } else {
            L = m;
        }
    }

    println!("{}", R);
}
