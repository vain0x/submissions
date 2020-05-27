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

fn solve1(s: &[i64], t: i64) -> bool {
    let mut map: BTreeSet<i64> = BTreeSet::new();
    let mut next = map.clone();
    map.insert(0);

    for d in s {
        next.clear();
        for &x in &map {
            next.insert(x + d);
            next.insert(x - d);
        }
        swap(&mut next, &mut map);
    }

    map.contains(&t)
}

fn main() {
    let s = rl();
    let (mut x, y) = read!(i64, i64);

    let mut first = true;

    // 0: x, 1: y
    let mut d = 0;
    let mut rep = 0;

    let mut xs = vec![];
    let mut ys = vec![];

    for c in s.chars() {
        match c {
            'F' => {
                if first {
                    x -= 1;
                    continue;
                }

                rep += 1;
            }
            'T' => {
                if d == 0 {
                    xs.push(rep);
                } else {
                    ys.push(rep);
                }
                d = 1 - d;
                rep = 0;
                first = false;
            }
            _ => unreachable!(),
        }
    }

    if d == 0 {
        xs.push(rep);
    } else {
        ys.push(rep);
    }

    let ok = solve1(&xs, x) && solve1(&ys, y);

    println!("{}", if ok { "Yes" } else { "No" })
}
