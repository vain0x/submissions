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

fn main() {
    let (H, W) = read!(usize, usize);
    let board = read![String; H];

    let board = board
        .into_iter()
        .map(|row| row.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();

    let mut s = (H, W);
    let mut g = (H, W);

    for y in 0..H {
        for x in 0..W {
            match board[y][x] {
                's' => s = (y, x),
                'g' => g = (y, x),
                _ => {}
            }
        }
    }

    let (sy, sx) = s;
    let (gy, gx) = g;

    let mut done = vec![vec![false; W]; H];
    done[sy][sx] = true;

    let mut q = VecDeque::new();
    q.push_back(s);

    let mut ok = false;

    while let Some((y, x)) = q.pop_front() {
        if y == gy && x == gx {
            ok = true;
            break;
        }

        for &(dy, dx) in &[(-1, 0), (0, -1), (1, 0), (0, 1)] {
            let (y2, x2) = (y as isize + dy, x as isize + dx);
            if y2 < 0 || y2 >= H as isize || x2 < 0 || x2 >= W as isize {
                continue;
            }

            let (y2, x2) = (y2 as usize, x2 as usize);

            if done[y2][x2] || board[y2][x2] == '#' {
                continue;
            }

            q.push_back((y2, x2));
            done[y2][x2] = true;
        }
    }

    println!("{}", if ok { "Yes" } else { "No" })
}
