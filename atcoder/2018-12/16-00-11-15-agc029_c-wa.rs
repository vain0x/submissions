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

    let mut u = A.clone();
    u.sort();
    u.dedup();

    let w = (0..u.len())
        .map(|i| if i == 0 { u[i] } else { u[i] - u[i - 1] } as usize)
        .collect::<Vec<_>>();
    debug!(w);

    let mut a_to_i = HashMap::new();
    for (i, &a) in u.iter().enumerate() {
        a_to_i.insert(a, i);
    }

    let mut pow = vec![];

    let mut ok = 1_000_000_000_i64;
    let mut ng = 0;
    while ok - ng > 1 {
        let m = ok + (ng - ok) / 2;
        let mut good = true;

        pow.clear();
        let mut p = std::num::Wrapping(1_i64);
        for _ in 0..60 {
            pow.push(p.0 as i64);
            let q = p * std::num::Wrapping(m);
            if q < p {
                break;
            }
            p = q;
        }

        let mut v = vec![0; u.len()];

        let mut it = A.iter().cloned().map(|a| a_to_i[&a]);
        let mut prev = it.next().unwrap();
        'fin: for next in it {
            if prev >= next {
                if m == 1 {
                    good = false;
                    break 'fin;
                }

                if (w[next] as usize) < pow.len() {
                    let mut i = next;
                    loop {
                        v[i] += 1;
                        if v[i] < pow[w[i] as usize] {
                            break;
                        }

                        if i == 0 {
                            good = false;
                            break 'fin;
                        }

                        v[i] = 0;
                        i -= 1;
                    }
                }
            }

            prev = next;
        }

        if good {
            ok = m;
        } else {
            ng = m;
        }

        debug!((m, pow.len(), good, ok, ng, v));
    }

    println!("{}", ok)
}
