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
fn brute_force() {
    struct BruteForce {
        H: usize,
        W: usize,
        A: usize,
        B: usize,
        board: Vec<Vec<u8>>,
    }

    impl BruteForce {
        fn row_ok(&self) -> bool {
            (0..self.H).all(|y| {
                let z = (0..self.W).filter(|&x| self.board[y][x] == 0).count();
                min(z, self.W - z) == self.A
            })
        }

        fn col_ok(&self) -> bool {
            (0..self.W).all(|x| {
                let z = (0..self.H).filter(|&y| self.board[y][x] == 0).count();
                min(z, self.H - z) == self.B
            })
        }

        fn println(&self) {
            println!("{}x{} ({}, {})", self.H, self.W, self.A, self.B);
            for y in 0..self.H {
                for x in 0..self.W {
                    print!("{}", self.board[y][x]);
                }
                print!("\n");
            }
        }

        fn dfs(&mut self, mut y: usize, mut x: usize) -> bool {
            if x == self.W {
                y += 1;
                x = 0;
            }

            if y == self.H {
                return self.row_ok() && self.col_ok() && {
                    self.println();
                    true
                };
            }

            assert_eq!(self.board[y][x], 0);
            if self.dfs(y, x + 1) {
                return true;
            }

            self.board[y][x] = 1;
            let ok = self.dfs(y, x + 1);
            self.board[y][x] = 0;
            ok
        }
    }

    for H in 5..7 {
        for W in 5..7 {
            let board = vec![vec![0; W]; H];
            let mut it = BruteForce {
                H: H,
                W: W,
                A: 0,
                B: 0,
                board: board,
            };

            for A in 2..H / 2 + 1 {
                for B in 2..H / 2 + 1 {
                    it.A = A;
                    it.B = B;
                    it.dfs(0, 0);
                }
            }
        }
    }
}
*/

fn main() {
    // brute_force();

    let (H, W, A, B) = read!(usize, usize, usize, usize);
    let mut board = vec![vec![0_u8; W]; H];

    for y in 0..H {
        for x in 0..W {
            board[y][x] = if y < B && x < A || y >= B && x >= A {
                1
            } else {
                0
            };
        }
    }

    for y in 0..H {
        for x in 0..W {
            print!("{}", board[y][x]);
        }
        print!("\n");
    }
}
