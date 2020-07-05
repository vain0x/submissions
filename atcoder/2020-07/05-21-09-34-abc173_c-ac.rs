//! Framework <https://github.com/vain0x/procon>

#![allow(non_snake_case)]
#![allow(unused_imports)]

use std::collections::*;

pub struct Scan(Box<dyn Iterator<Item = &'static str>>); // '

impl Scan {
    fn new() -> Self {
        let mut buf = String::new();
        let read_line = move || {
            std::io::stdin().read_line(&mut buf).unwrap();
            Box::leak(buf.split_off(0).into_boxed_str()).split_whitespace()
        };
        Scan(Box::new(std::iter::repeat_with(read_line).flatten()))
    }

    pub fn word<T: std::str::FromStr>(&mut self) -> T {
        self.0.next().unwrap().parse().ok().unwrap()
    }

    pub fn list<T: std::str::FromStr>(&mut self, len: usize) -> Vec<T> {
        std::iter::repeat_with(|| self.word()).take(len).collect()
    }
}

#[macro_export]
macro_rules! grid_vec {
    ($item:expr; $h:expr, $w:expr) => {{
        let item = $item;
        let mut it = GridVec::new();
        it.resize($h, $w, item);
        it
    }};
}

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
        // '
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
    owner: &'a GridVec<T>, // '
    y: usize,
    x: usize,
    i: usize,
}

impl<'a, T> Iterator for GridEnumerateIter<'a, T> {
    type Item = ((usize, usize), &'a T); // '

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
    let mut scan = Scan::new();

    let H = scan.word::<usize>();
    let W = scan.word::<usize>();
    let K = scan.word::<usize>();

    let mut board = GridVec::new();
    for _ in 0..H {
        board.push_row(scan.word::<String>().chars());
    }

    let total_black = board.enumerate().filter(|(_, c)| **c == '#').count();
    let mut n = 0;

    for s in 0..1 << H {
        let mut black = total_black;

        for y in 0..H {
            if (s >> y) & 1 == 0 {
                continue;
            }

            for x in 0..W {
                if board[(y, x)] != '#' {
                    continue;
                }

                black -= 1;
            }
        }

        for t in 0..1 << W {
            let mut black = black;

            for x in 0..W {
                if (t >> x) & 1 == 0 {
                    continue;
                }

                for y in 0..H {
                    if (s >> y) & 1 != 0 {
                        continue;
                    }

                    if board[(y, x)] == '#' {
                        black -= 1;
                    }
                }
            }

            if black == K {
                n += 1_usize;
            }
        }
    }

    println!("{}", n);
}
