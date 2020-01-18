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

5
3 1 4 1 5
3 1 4 1 5
3

5
3 1 4 1 5
9 2 6 5 3
-1

5
1 4 1 4 2
2 2 3 6 6
3

*/

fn neg(x: &mut bool) {
    *x = !*x;
}

fn main() {
    let N = read!(usize);
    let A = read![[i64]];
    let B = read![[i64]];

    let mut numbers = vec![Default::default(); N];
    let mut cards = vec![Default::default(); N];
    let mut parity = vec![Default::default(); N];
    let mut min_cost = std::i64::MAX;

    // 各カードの移動回数の偶奇を決め打ちすることで結果の数列を決定する。
    for s in 0..1 << N {
        let flip = |i: usize| (s & (1 << i)) != 0;

        for i in 0..N {
            let c = if flip(i) { B[i] } else { A[i] };
            cards[i] = (c, i);
            numbers[i] = c;
            parity[i] = false;
        }

        numbers.sort();

        // 選択ソートによりカードの表面の数字の列を numbers に一致させる。

        let mut l = 0;
        let mut cost = 0;
        let mut ok = true;

        while l < N {
            // 左端の数値に一致する最初のカードを見つける。ただし偶奇が壊れるものは除く。
            let i = match (l..N)
                .filter(|&i| {
                    cards[i].0 == numbers[l]
                        && parity[cards[i].1] ^ ((i - l) % 2 != 0) == flip(cards[i].1)
                })
                .next()
            {
                None => {
                    ok = false;
                    break;
                }
                Some(i) => i,
            };

            for j in (l..i).rev() {
                cards.swap(j, j + 1);
                neg(&mut parity[cards[j].1]);
                neg(&mut parity[cards[j + 1].1]);
                cost += 1;
            }

            debug_assert_eq!(cards[l].0, numbers[l]);
            if parity[cards[l].1] != flip(cards[l].1) || cost > min_cost {
                ok = false;
                break;
            }
            l += 1;
        }

        if ok {
            min_cost = min(min_cost, cost);
        }
    }

    if min_cost == std::i64::MAX {
        println!("-1")
    } else {
        println!("{}", min_cost)
    }
}
