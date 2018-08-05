#![allow(unused_imports)]
#![allow(non_snake_case)]

use std::cell::RefCell;
use std::cmp::{max, min, Ordering};
use std::collections::*;
use std::fmt::{Debug, Formatter};
use std::io::*;
use std::mem::{replace, swap};
use std::ops::*;
use std::rc::Rc;

// -----------------------------------------------
// Framework
// -----------------------------------------------

#[allow(unused_macros)]
macro_rules! read {
    ([$t:ty] ; $n:expr) =>
        ((0..$n).map(|_| read!([$t])).collect::<Vec<_>>());
    ($($t:ty),+ ; $n:expr) =>
        ((0..$n).map(|_| read!($($t),+)).collect::<Vec<_>>());
    ([$t:ty]) =>
        (rl().split_whitespace().map(|w| w.parse().unwrap()).collect::<Vec<$t>>());
    ($t:ty) =>
        (rl().parse::<$t>().unwrap());
    ($($t:ty),*) => {{
        let buf = rl();
        let mut w = buf.split_whitespace();
        ($(w.next().unwrap().parse::<$t>().unwrap()),*)
    }};
}

#[allow(unused_macros)]
macro_rules! debug {
    ($($arg:expr),*) => {
        #[cfg(debug_assertions)]
        {
            let entries = [$(&stringify!([$arg]:), &$arg as &Debug),*];
            stderr().write_fmt(format_args!("{:#?}\n", entries)).unwrap();
        }
    };
}

#[allow(dead_code)]
fn rl() -> String {
    let mut buf = String::new();
    stdin().read_line(&mut buf).unwrap();
    buf.trim_right().to_owned()
}

trait IteratorExt: Iterator + Sized {
    fn vec(self) -> Vec<Self::Item> {
        self.collect()
    }
}

impl<T: Iterator> IteratorExt for T {}

// -----------------------------------------------
// Solution
// -----------------------------------------------

pub fn main() {
    let (H, W) = read!(usize, usize);
    let S = read![String; H];
    let board_core = S
        .into_iter()
        .map(|s| s.chars().map(|c| c != '#').vec())
        .vec();

    let mut board = vec![vec![false; W + 2]; H + 2];
    for y in 0..H {
        for x in 0..W {
            board[y + 1][x + 1] = board_core[y][x];
        }
    }

    // su[y][x]: y,x とその上方向に連続して存在する空きマスの数
    let mut su = vec![vec![0; W + 3]; H + 3];

    let mut sd = vec![vec![0; W + 3]; H + 3];
    let mut sr = vec![vec![0; W + 3]; H + 3];
    let mut sl = vec![vec![0; W + 3]; H + 3];

    for x in 1..W + 1 {
        for y in 1..H + 1 {
            su[y][x] += if board[y][x] { su[y - 1][x] + 1 } else { 0 };
        }

        for y in (1..H + 1).rev() {
            sd[y][x] += if board[y][x] { sd[y + 1][x] + 1 } else { 0 };
        }
    }

    for y in 1..H + 1 {
        for x in 1..W + 1 {
            sl[y][x] += if board[y][x] { sl[y][x - 1] + 1 } else { 0 };
        }

        for x in (1..W + 1).rev() {
            sr[y][x] += if board[y][x] { sr[y][x + 1] + 1 } else { 0 };
        }
    }

    let space_count = |d, y: usize, x: usize| match d {
        0 => su[y][x],
        1 => sl[y][x],
        2 => sd[y][x],
        3 => sr[y][x],
        _ => unreachable!(),
    } - 1;

    let mut sum = 0_i64;
    for y in 1..H + 1 {
        for x in 1..W + 1 {
            if !board[y][x] {
                continue;
            }

            for d1 in 0..4 {
                let d2 = (d1 + 1) % 4;
                sum += space_count(d1, y, x) * space_count(d2, y, x);
            }
        }
    }

    println!("{}", sum);
    return;
}
