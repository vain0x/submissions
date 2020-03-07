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

5 3
31415
//=> 4

20 7
31415314153141531415
//=> 23

15 2
314159265358979
//=> 30

15 3
314159265358979
//=> 35

15 5
314159265358979
//=> 25

15 7
314159265358979
//=> 12

15 11
314159265358979
//=> 5
*/

fn ch(c: char) -> i64 {
    (c as u8 - b'0') as i64
}

fn solve(S: &str, P: i64) -> usize {
    let S = S.chars().collect::<Vec<_>>();
    let N = S.len();

    if P == 2 {
        // 右端が偶数である区間の個数
        return (0..N)
            .map(|i| if ch(S[i]) % 2 == 0 { i + 1 } else { 0 })
            .sum::<usize>();
    }

    if P == 5 {
        // 右端が 0 または 5 である区間の個数
        return (0..N)
            .map(|i| if S[i] == '0' || S[i] == '5' { i + 1 } else { 0 })
            .sum::<usize>();
    }

    let mut T = vec![0; N + 1];
    let mut h = 1;
    for r in (0..N).rev() {
        let d = ch(S[r]);
        T[r] = (d * h + T[r + 1]) % P;
        h = (h * 10) % P;
    }

    let mut freq = vec![0_usize; P as usize];
    for i in 0..T.len() {
        freq[T[i] as usize] += 1;
    }

    debug!(freq, T);

    let mut total = 0_usize;
    for i in 0..freq.len() {
        if freq[i] >= 1 {
            total += freq[i] * (freq[i] - 1) / 2;
        }
    }
    total
}

fn main() {
    let (_, P) = read!(usize, i64);
    let S = rl();
    println!("{}", solve(&S, P));
}

#[cfg(test)]
mod tests {
    use super::*;

    fn brute_force(S: &str, P: i64) -> usize {
        let S = S.chars().collect::<Vec<_>>();
        let mut count = 0_usize;

        for l in 0..S.len() {
            for r in l + 1..S.len() + 1 {
                let mut x = 0;
                for i in l..r {
                    x = (x * 10 + ch(S[i])) % P;
                }

                if x % P == 0 {
                    debug!((l, r, x));
                    count += 1;
                }
            }
        }

        count
    }

    #[test]
    fn samples() {
        assert_eq!(brute_force("31415314153141531415", 7), 23);

        assert_eq!(brute_force("314159265358979", 2), 30);
        assert_eq!(brute_force("314159265358979", 3), 35);
        assert_eq!(brute_force("314159265358979", 5), 25);
        assert_eq!(brute_force("314159265358979", 7), 12);
        assert_eq!(brute_force("314159265358979", 11), 5);
    }
}
