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

fn main() {
    let N = read!(usize);
    let mut A = read![[usize]; N];

    // opponents[u][0] : 選手 u の次の対戦相手
    let mut opponents = vec![VecDeque::new(); N];
    // 昨日試合を行った選手
    let mut prev = (0..N).collect::<BTreeSet<_>>();
    // 本日試合を行う選手
    let mut next = BTreeSet::new();

    for u in 0..N {
        for i in 0..A[u].len() {
            A[u][i] -= 1;

            opponents[u].push_back(A[u][i]);
        }
    }

    let mut day = 0_usize;

    loop {
        debug!(day);

        next.clear();

        for &u in prev.iter() {
            let v = match opponents[u].get(0) {
                None => continue,
                Some(&v) => v,
            };

            let w = match opponents[v].get(0) {
                None => unreachable!(),
                Some(&w) => w,
            };

            // v が u 以外の選手を指名しているなら u は試合できない。
            if u != w {
                continue;
            }

            // 互いに次の対戦相手に指名している選手同士の試合を組む。
            debug!((u, v));
            next.insert(u);
            next.insert(v);
        }

        // 試合がないなら終わり
        if next.is_empty() {
            break;
        }

        for &u in next.iter() {
            opponents[u].pop_front();
        }

        swap(&mut next, &mut prev);

        day += 1;
    }

    // 試合を組めなければ矛盾している
    let ok = (0..N).all(|u| opponents[u].is_empty());
    if !ok {
        println!("-1");
        return;
    }

    println!("{}", day)
}
