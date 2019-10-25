//! ----------------------------------------------
//! Framework <https://github.com/vain0x/procon>
//!
//! See the bottom of file for solution.
//! ----------------------------------------------

#![allow(unused_imports)]
#![allow(non_snake_case)]

use std::cell::RefCell;
use std::cmp::{max, min, Ordering};
use std::collections::*;
use std::fmt::{Debug, Display, Formatter, Write as FmtWrite};
use std::io::{stderr, stdin, BufRead, Write};
use std::mem::{replace, swap};
use std::ops::*;
use std::rc::Rc;

/// Print values to standard error if debug mode.
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

/// Read from standard input and parse each word.
/// - `read!(T, U, ..)` parses a line as a tuple of words.
/// - `read![[T]]` parses a line as an array of words.
/// - `read![..; N]` parses `N` lines, using `read!(..)` repeatedly.
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

/// Read a line from standard input.
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

/*

a
aaa
//=> 3

contest
contest
//=> 7

*/

fn char_to_index(c: char) -> usize {
    (c as u8 - b'a') as usize
}

fn main() {
    let S = rl().chars().collect::<Vec<_>>();
    let T = rl().chars().collect::<Vec<_>>();

    let SN = S.len();
    let TN = T.len();

    // next[si][ci] = (ni, wrap)
    // S の位置 si から見て次に文字 ci が出現する位置が ni
    // ni=si もありうる
    // wrap=1 なら次の周回
    let mut next = vec![vec![None; 26]; SN];

    let mut last = vec![None; 26];

    for i in (0..SN).rev() {
        let c = S[i];
        last[char_to_index(c)] = Some(i);
    }

    for i in (0..SN).rev() {
        let c = S[i];
        last[char_to_index(c)] = Some(i);

        for ci in 0..26 {
            next[i][ci] = match last[ci] {
                None => None,
                Some(next_i) => Some((next_i, (if next_i < i { 1_usize } else { 0 }))),
            }
        }
    }

    // S を wrap 個つなげたのものに S(..si) をつなげたものが T(..ti) を部分列として含む
    let mut si = 0;
    let mut wrap = 0;
    let mut possible = true;
    for ti in 0..TN {
        let c = T[ti];
        match next[si][char_to_index(c)] {
            Some((next_i, w)) => {
                si = next_i;
                wrap += w;

                si += 1;
                if si == SN {
                    si = 0;
                    wrap += 1;
                }
            }
            None => {
                possible = false;
                break;
            }
        }

        debug!((ti, c, wrap, si));
    }

    if possible {
        println!("{}", wrap * SN + si)
    } else {
        println!("-1")
    }
}
