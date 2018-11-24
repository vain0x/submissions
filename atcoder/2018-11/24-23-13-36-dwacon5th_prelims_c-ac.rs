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

    for k in K {
        debug!(k);

        let mut total = 0_i64;

        // いまの区間に含まれる D の個数
        let mut d_count = 0_i64;

        // いまの区間に含まれる M の個数
        let mut m_count = 0_i64;

        // いまの区間に含まれる DM ペアの個数
        let mut dm_count = 0_i64;

        let mut l = 0;
        for r in 0..N {
            if r - l >= k {
                // 次の区間幅を k に抑えるため、区間先頭を除外する。
                if S[l] == 'D' {
                    d_count -= 1;
                    dm_count -= m_count;
                }
                if S[l] == 'M' {
                    m_count -= 1;
                }
                l += 1;
            }

            if S[r] == 'D' {
                d_count += 1;
            }
            if S[r] == 'M' {
                // 区間の末尾に M が出現することで、区間内の DM ペアの個数は D の個数だけ増える。
                m_count += 1;
                dm_count += d_count;
                debug!((r, S[r], d_count, m_count, dm_count));
            }
            if S[r] == 'C' {
                // 区間内の DM ペアの個数だけ DMC が見つかったので数える。
                total += dm_count;
                debug!((r, S[r], dm_count, total));
            }
        }

        println!("{}", total);
    }
}
