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

// -----------------------------------------------
// Framework <https://github.com/vain0x/procon>
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
        $(writeln!(stderr(), "{} = {:?}", stringify!($arg), $arg).unwrap());*
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

fn main() {
    let (H, W) = read!(usize, usize);
    let B = read![String; H].into_iter().map(|s| s.bytes().vec()).vec();

    let mut blue = vec![vec![b'.'; W]; H];
    let mut red = vec![vec![b'.'; W]; H];

    for y in 0..H {
        for x in 0..W {
            if B[y][x] == b'#' || y == 0 || (y < H - 1 && x % 2 == 0) {
                blue[y][x] = b'#';
            }

            if B[y][x] == b'#' || y == H - 1 || (y > 0 && x % 2 != 0) {
                red[y][x] = b'#';
            }
        }
    }

    fn print_board(board: &Vec<Vec<u8>>) {
        let stdout = std::io::stdout();
        let mut lock = stdout.lock();
        for line in board {
            lock.write(line).unwrap();
            lock.write(&[b'\n']).unwrap();
        }
        lock.flush().unwrap();
    }

    print_board(&blue);
    println!("");
    print_board(&red);

    // debug!({
    //     let mut cross = vec![vec![b'.'; W]; H];
    //     for y in 0..H {
    //         for x in 0..W {
    //             if blue[y][x] == b'#' && red[y][x] == b'#' {
    //                 cross[y][x] = b'#';
    //             }
    //         }
    //     }
    //     println!("");
    //     print_board(&cross);
    // });
}
