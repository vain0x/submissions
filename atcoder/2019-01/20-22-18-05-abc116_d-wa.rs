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
use std::fmt::{Debug, Display, Formatter, Write as FmtWrite};
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

fn main() {
    let (N, K) = read!(usize, usize);
    let T = read![usize, i64; N];

    let mut T = T.into_iter().map(|(t, d)| (d, t)).collect::<Vec<_>>();
    T.sort();
    T.reverse();

    debug!(T);

    let mut s = 0_i64;
    let mut x = 0_usize;
    let mut Q = vec![];

    // M[t] = true : ネタ t のスシを選んでいる
    let mut M = HashSet::new();

    for i in 0..K {
        let (d, t) = T[i];

        if M.contains(&t) {
            // 同じネタの2個目以降のスシは食べない可能性がある
            Q.push(d);
        } else {
            // 1個目のスシは必ず食べるので Q には入れない
            M.insert(t);
            x += 1;
        }

        s += d;
    }

    let mut max_score = s + x as i64 * x as i64;
    debug!(M, s, x, max_score);

    for i in K..N {
        let (d, t) = T[i];

        if M.contains(&t) {
            continue;
        }

        // このスシはまだ食べてないネタ

        // このスシを食べる。ネタが重複しているスシの中でもっともおいしくないものを捨てる。
        if let Some(d) = Q.pop() {
            s -= d;
        } else {
            // 捨てられるスシがないなら終了
            break;
        }

        x += 1;
        s += d;
        max_score = max(max_score, s + x as i64 * x as i64);
        debug!(d, s, x, x * x, max_score);
    }

    println!("{}", max_score);
}
