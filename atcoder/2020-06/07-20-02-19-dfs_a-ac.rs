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

macro_rules! grid_vec {
    ($item:expr; $h:expr, $w:expr) => {{
        let item = $item;
        let mut it = GridVec::new();
        it.resize($h, $w, item);
        it
    }};
}

/// Vec (dynamic array) of 2d index.
#[derive(Clone)]
pub struct GridVec<T> {
    h: usize,
    w: usize,
    data: Vec<T>,
}

impl<T> GridVec<T> {
    pub fn new() -> Self {
        GridVec {
            h: 0,
            w: 0,
            data: Vec::new(),
        }
    }

    pub fn clear(&mut self) {
        self.h = 0;
        self.w = 0;
        self.data.clear();
    }

    pub fn resize(&mut self, h: usize, w: usize, item: T)
    where
        T: Clone,
    {
        self.h = h;
        self.w = w;
        self.data.clear();
        self.data.resize(h * w, item);
    }

    pub fn push_row<IntoIter: IntoIterator<Item = T>>(&mut self, row: IntoIter)
    where
        T: Clone + Default,
    {
        if self.h == 0 {
            self.data.extend(row);
            self.h = 1;
            self.w = self.data.len();
        } else {
            self.h += 1;
            let w = self.w;
            self.data.extend(row.into_iter().take(w));
            self.data.resize(self.h * w, T::default());
        }
    }

    pub fn get(&self, index: (usize, usize)) -> Option<&T> {
        let i = index.0 * self.w + index.1;
        self.data.get(i)
    }

    pub fn get_mut(&mut self, index: (usize, usize)) -> Option<&mut T> {
        let i = index.0 * self.w + index.1;
        self.data.get_mut(i)
    }

    pub fn enumerate<'a>(&'a self) -> GridEnumerateIter<'a, T> {
        GridEnumerateIter {
            owner: self,
            y: 0,
            x: 0,
            i: 0,
        }
    }

    pub fn neighbor(
        &self,
        index: (usize, usize),
        offset: (isize, isize),
    ) -> Option<(usize, usize)> {
        let y = (index.0 as isize + offset.0) as usize;
        let x = (index.1 as isize + offset.1) as usize;
        if y < self.h && x < self.w {
            Some((y, x))
        } else {
            None
        }
    }

    pub fn four_neighbors(&self, index: (usize, usize)) -> Vec<(usize, usize)> {
        [(0, -1), (0, 1), (-1, 0), (1, 0)]
            .iter()
            .filter_map(|&offset| self.neighbor(index, offset))
            .collect()
    }

    pub fn eight_neighbors(&self, index: (usize, usize)) -> Vec<(usize, usize)> {
        [
            (-1, -1),
            (-1, 0),
            (-1, 1),
            (0, -1),
            (0, 1),
            (1, -1),
            (1, 0),
            (1, 1),
        ]
        .iter()
        .filter_map(|&offset| self.neighbor(index, offset))
        .collect()
    }
}

pub struct GridEnumerateIter<'a, T: 'a> {
    owner: &'a GridVec<T>,
    y: usize,
    x: usize,
    i: usize,
}

impl<'a, T> Iterator for GridEnumerateIter<'a, T> {
    type Item = ((usize, usize), &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        let (y, x, i) = (self.y, self.x, self.i);
        if y == self.owner.h {
            return None;
        }

        self.i += 1;
        self.x += 1;
        if self.x == self.owner.w {
            self.x = 0;
            self.y += 1;
        }

        let item = unsafe { self.owner.data.get_unchecked(i) };
        Some(((y, x), item))
    }
}

impl<T> std::ops::Index<(usize, usize)> for GridVec<T> {
    type Output = T;

    fn index(&self, index: (usize, usize)) -> &T {
        let i = index.0 * self.w + index.1;
        &self.data[i]
    }
}

impl<T> std::ops::IndexMut<(usize, usize)> for GridVec<T> {
    fn index_mut(&mut self, index: (usize, usize)) -> &mut T {
        let i = index.0 * self.w + index.1;
        &mut self.data[i]
    }
}

fn main() {
    let (H, W) = read!(usize, usize);

    let mut board = GridVec::new();
    for row in read![String; H] {
        board.push_row(row.chars());
    }

    let mut s = (H, W);
    let mut g = (H, W);

    for (i, &c) in board.enumerate() {
        match c {
            's' => s = i,
            'g' => g = i,
            _ => {}
        }
    }

    let mut done: GridVec<bool> = grid_vec![false; H, W];
    done[s] = true;

    let mut q = VecDeque::new();
    q.push_back(s);

    let mut ok = false;
    while let Some(u) = q.pop_front() {
        if u == g {
            ok = true;
            break;
        }

        for v in board.four_neighbors(u) {
            if done[v] || board[v] == '#' {
                continue;
            }

            q.push_back(v);
            done[v] = true;
        }
    }

    println!("{}", if ok { "Yes" } else { "No" })
}
