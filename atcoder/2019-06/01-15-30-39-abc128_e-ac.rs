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

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Rev<T>(pub T);

impl<T: PartialOrd> PartialOrd for Rev<T> {
    fn partial_cmp(&self, other: &Rev<T>) -> Option<Ordering> {
        other.0.partial_cmp(&self.0)
    }
}

impl<T: Ord> Ord for Rev<T> {
    fn cmp(&self, other: &Rev<T>) -> Ordering {
        other.0.cmp(&self.0)
    }
}

#[derive(Clone, Copy, Debug)]
enum Event {
    /// 位置 x を通行止め集合に入れる
    Push(i64),
    /// 位置 x を通行止め集合から取り除く
    Pop(i64),
    /// 人 i の歩行距離を計算する
    Query(usize),
}

fn main() {
    let (N, Q) = read!(usize, usize);
    let T = read![i64, i64, i64; N];
    let D = read![i64; Q];

    let mut E = vec![];
    for (s, t, x) in T {
        // 時刻 s - x に位置 x が通行止めになる
        E.push((s - x, Event::Push(x)));
        // 時刻 t - x に位置 x が通行止めでなくなる
        E.push((t - x, Event::Pop(x)));
    }
    for i in 0..D.len() {
        // 時刻 D(i) に人 i が歩き出す
        E.push((D[i], Event::Query(i)));
    }

    // 時刻についてソート
    E.sort_by(|&(ref lt, _), &(ref rt, _)| lt.cmp(rt));

    // 通行止めの集合 (最小値を取りたいのでヒープ)
    let mut queue = BinaryHeap::new();

    // 通行止めの集合から除去された要素の多重集合
    let mut removal = BTreeMap::new();

    // W[i] = i番目の人の歩行距離
    let mut W = vec![0; Q];

    for (_, e) in E {
        match e {
            Event::Push(x) => {
                queue.push(Rev(x));
            }
            Event::Pop(x) => {
                // removal[x] の値を1増やす
                *removal.entry(x).or_insert(0) += 1;
            }
            Event::Query(i) => {
                let mut min_x = -1;

                // ヒープから最小値を取る
                // ただし除外要素がトップにあるときは除去する
                while let Some(&Rev(x)) = queue.peek() {
                    match removal.get(&x) {
                        Some(&n) if n > 0 => {
                            // x は除外要素
                            queue.pop();
                            *removal.get_mut(&x).unwrap() -= 1;
                            continue;
                        }
                        _ => {
                            min_x = x;
                            break;
                        }
                    }
                }

                W[i] = min_x;
            }
        }
    }

    for w in W {
        println!("{}", w);
    }
}
