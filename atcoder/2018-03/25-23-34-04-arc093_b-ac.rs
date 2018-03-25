#![allow(unused_imports)]
#![allow(non_snake_case)]

use std::*;
use std::collections::*;
use std::ops::*;
use procon::*;
use self::Cell::*;

#[derive(Clone, Copy)]
enum Cell {
    White,
    Black,
}

pub fn main() {
    let (W, B) = {
        let x = read_words::<i32>();
        (x[0], x[1])
    };

    let mut board = vec![vec![White; 100]; 100];
    for y in 0..100 {
        for x in 50..100 {
            board[y][x] = Black;
        }
    }

    let mut w = W - 1;
    let mut b = B - 1;
    for yi in 0..50 {
        for xi in 0..25 {
            {
                let y = yi * 2;
                let x = xi * 2;
                if b > 0 {
                    board[y][x] = Black;
                    b -= 1;
                }
            }

            {
                let y = yi * 2;
                let x = 51 + xi * 2;
                if w > 0 {
                    board[y][x] = White;
                    w -= 1;
                }
            }
        }
    }

    println!("100 100");
    for row in board {
        let line = row.into_iter()
            .map(|c| match c {
                White => '.',
                Black => '#',
            })
            .collect::<String>();
        println!("{}", line);
    }

    return;
}

pub mod procon {
    use std;
    use std::collections::*;
    use std::io;
    use std::mem;
    use std::str::FromStr;

    pub fn read_line() -> String {
        let mut buf = String::new();
        io::stdin().read_line(&mut buf).unwrap();
        buf.trim_right().to_owned()
    }

    pub fn read_words<T>() -> Vec<T>
    where
        T: std::str::FromStr,
        T::Err: std::fmt::Debug,
    {
        let mut buf = String::new();
        io::stdin().read_line(&mut buf).unwrap();
        buf.split_whitespace()
            .map(|word| T::from_str(word).unwrap())
            .collect()
    }

    pub fn read_vec(len: usize) -> Vec<String> {
        let mut vec = Vec::new();
        while vec.len() < len {
            let line = read_line();
            for word in line.split_whitespace() {
                vec.push(word.to_owned());
            }
        }
        assert!(vec.len() == len);
        vec
    }
}
