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

struct Solver {
    S: Vec<char>,
    T: Vec<(char, char)>,
    done: Vec<bool>,
    L: BTreeMap<char, BTreeSet<usize>>,
    R: BTreeMap<char, BTreeSet<usize>>,
}

impl Solver {
    fn add(&mut self, i: usize) {
        self.done[i] = true;

        if i >= 1 {
            let lt = self.S[i - 1];
            self.L.get_mut(&lt).unwrap().insert(i);
        }
        if i + 1 < self.S.len() {
            let rt = self.S[i + 1];
            self.R.get_mut(&rt).unwrap().insert(i);
        }
    }

    fn remove(&mut self, i: usize) {
        if i >= 1 {
            let lt = self.S[i - 1];
            self.L.get_mut(&lt).unwrap().remove(&i);
        }
        if i + 1 < self.S.len() {
            let rt = self.S[i + 1];
            self.R.get_mut(&rt).unwrap().remove(&i);
        }
    }

    fn run(&mut self) {
        let N = self.S.len();
        for (t, d) in self.T.clone().into_iter().rev() {
            if d == 'L' {
                let set = replace(self.R.get_mut(&t).unwrap(), BTreeSet::new());
                debug!(set);

                for &i in &set {
                    self.remove(i);
                }

                for i in set {
                    if i + 1 < N {
                        self.add(i + 1);
                    }
                }

                if self.S[0] == t {
                    self.add(0);
                }
            } else {
                let set = replace(self.L.get_mut(&t).unwrap(), BTreeSet::new());
                debug!(set);

                for &i in &set {
                    self.remove(i);
                }

                for i in set {
                    if i >= 1 {
                        self.add(i - 1);
                    }
                }

                if self.S[N - 1] == t {
                    self.add(N - 1);
                }
            }

            debug!((t, d), self.L, self.R);
        }

        let die = (0..N).filter(|&i| self.done[i]).count();

        println!("{}", N - die)
    }
}

fn main() {
    let (N, Q) = read!(usize, usize);
    let S = rl().chars().collect::<Vec<_>>();
    let T = read![String, String; Q]
        .into_iter()
        .map(|(t, d)| {
            let t = t.chars().next().unwrap();
            let d = d.chars().next().unwrap();
            (t, d)
        })
        .collect();

    let mut map = BTreeMap::new();
    for i in 0..26 {
        map.insert(('A' as u8 + i) as char, BTreeSet::new());
    }

    Solver {
        S: S,
        T: T,
        done: vec![false; N],
        L: map.clone(),
        R: map.clone(),
    }
    .run();
}
