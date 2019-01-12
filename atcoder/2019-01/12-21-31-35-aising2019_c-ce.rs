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

struct Dfs<'a> {
    H: usize,
    W: usize,
    done: Vec<Vec<bool>>,
    board: &'a Vec<Vec<char>>,
}

impl<'a> Dfs<'a> {
    fn run(mut self) {
        let mut s = 0_i64;

        for y in 0..self.H {
            for x in 0..self.W {
                if self.done[y][x] {
                    continue;
                }

                let mut whites = 0_i64;
                let mut blacks = 0;

                self.dfs(y, x, &mut whites, &mut blacks);

                debug!((y, x, whites, blacks));

                s += whites * blacks;
            }
        }

        println!("{}", s)
    }

    fn dfs(&mut self, y: usize, x: usize, whites: &mut i64, blacks: &mut i64) {
        if self.done[y][x] {
            return;
        }
        self.done[y][x] = true;

        if self.board[y][x] == '.' {
            *whites += 1;
        } else {
            *blacks += 1;
        }

        if y >= 1 {
            if self.board[y][x] != self.board[y - 1][x] {
                self.dfs(y - 1, x, whites, blacks);
            }
        }
        if x >= 1 {
            if self.board[y][x] != self.board[y][x - 1] {
                self.dfs(y, x - 1, whites, blacks);
            }
        }
        if y + 1 < self.H {
            if self.board[y][x] != self.board[y + 1][x] {
                self.dfs(y + 1, x, whites, blacks);
            }
        }
        if x + 1 < self.W {
            if self.board[y][x] != self.board[y][x + 1] {
                self.dfs(y, x + 1, whites, blacks);
            }
        }
    }
}

fn main() {
    let (H, W) = read!(usize, usize);
    let board = read![String; H]
        .into_iter()
        .map(|s| s.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();

    Dfs {
        H,
        W,
        board: &board,
        done: vec![vec![false; W]; H],
    }
    .run()
}
