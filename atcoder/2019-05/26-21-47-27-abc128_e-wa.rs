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

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Ev {
    Push(i64),
    Pop(i64),
}

fn main() {
    let (N, Q) = read!(usize, usize);
    let T = read![i64, i64, i64; N];
    let D = read![i64; Q];

    let mut E = vec![];
    for (s, t, x) in T {
        E.push((s - x, Ev::Push(x)));
        E.push((t - x, Ev::Pop(x)));
    }
    E.sort();
    // debug!(E);
    let mut E = E.into_iter().collect::<VecDeque<_>>();

    let mut queue = BinaryHeap::new();
    let mut drops = BTreeSet::new();

    for d in D {
        // debug!(d);
        loop {
            match E.get(0) {
                Some(&(next_t, ev)) if next_t <= d => {
                    match ev {
                        Ev::Push(x) => {
                            if !drops.remove(&x) {
                                // debug!("push", x);
                                queue.push(Rev(x));
                            }
                        }
                        Ev::Pop(x) => {
                            // debug!("pop", x);
                            drops.insert(x);
                        }
                    }
                    E.pop_front();
                }
                _ => {
                    break;
                }
            }
        }

        let mut x = -1;
        while let Some(&Rev(v)) = queue.peek() {
            if drops.remove(&v) {
                queue.pop();
                continue;
            }
            x = v;
            break;
        }

        println!("{}", x)
    }
}
