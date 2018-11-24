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
    let N = read!(usize);
    let S = rl().chars().collect::<Vec<_>>();
    let _Q = read!(usize);
    let K = read![[usize]];

    // 累積和
    let mut a_sum = vec![0; N + 1];
    {
        for i in 0..N {
            if S[i] == 'D' {
                a_sum[1 + i] += 1;
            }
        }
        for i in 1..a_sum.len() {
            a_sum[i] += a_sum[i - 1];
        }
    }

    let mut b_sum = vec![0; N + 1];
    {
        for i in 0..N {
            if S[i] == 'M' {
                b_sum[1 + i] += 1;
            }
        }
        for i in 1..b_sum.len() {
            b_sum[i] += b_sum[i - 1];
        }
    }

    let mut c_sum = vec![0; N + 1];
    {
        for i in 0..N {
            if S[i] == 'C' {
                c_sum[1 + i] += 1;
            }
        }
        for i in 1..c_sum.len() {
            c_sum[i] += c_sum[i - 1];
        }
    }

    debug!(a_sum, b_sum, c_sum);

    for k in K {
        debug!(k);
        let mut s = 0_i64;

        // if k * k <= N

        // 幅 w のトリプルを数える (O(N^2))
        for w in 3..k + 1 {
            for l in 0..N {
                if S[l] != 'D' {
                    continue;
                }
                let r = l + w;
                if r > N {
                    continue;
                }
                if S[r - 1] != 'C' {
                    continue;
                }
                s += b_sum[r - 1] - b_sum[l + 1];
            }
        }

        // for i in 1..N - 1 {
        //     if S[i] == 'M' {
        //         debug!((i, a_sum[i], c_sum[N], c_sum[i + 1]));
        //         s += a_sum[i] * (c_sum[N] - c_sum[i + 1]);
        //     }
        // }
        // debug!(s);

        // // a に対してペアになれない c を数える
        // let mut x_sum = vec![0; N + 1];
        // for i in 0..N {
        //     if i + k >= N {
        //         continue;
        //     }
        //     if S[i] == 'D' {
        //         let c = c_sum[N] - c_sum[i + k];
        //         x_sum[1 + i] += c_sum[N] - c_sum[i + k];
        //         x_sum[i + k] -= c; // FIXME: dame
        //         debug!((i, x_sum[1 + i], c_sum[i + k]));
        //     }
        // }
        // for i in 1..x_sum.len() {
        //     x_sum[i] += x_sum[i - 1];
        // }
        // debug!(x_sum);

        // for i in 1..N {
        //     if S[i] == 'M' {
        //         s -= x_sum[i];
        //         debug!((i, x_sum[i], s));
        //     }
        // }

        println!("{}", s);
    }
}
