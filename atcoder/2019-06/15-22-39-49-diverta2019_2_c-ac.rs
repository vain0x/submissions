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

/*

4
-1 -1 -1 2
#=> 5

3
-1 -1 -1
#=> 1

6
3 1 4 1 5 9
# => 13

*/

fn brute_force(A: &[i64]) -> i64 {
    if A.len() <= 1 {
        return A[0];
    }

    let mut score = std::i64::MIN;

    let mut B = A.to_owned();
    for i in 0..A.len() {
        for j in i + 1..A.len() {
            B.clear();
            B.extend(&A[0..i]);
            B.extend(&A[i + 1..j]);
            B.extend(&A[j + 1..]);

            B.push(A[i] - A[j]);
            score = max(score, brute_force(&B));

            B.pop();
            B.push(A[j] - A[i]);
            score = max(score, brute_force(&B));
        }
    }
    score
}

// fn solve(A: &[i64]) -> i64 {
//     // A: sorted
//     let N = A.len();

//     let mut ops = vec![];
//     let mut xs = vec![];
//     let mut ys = vec![];

//     let mut h = 1;
//     while h + 1 < N && A[h] < 0 {
//         h += 1;
//     }

//     for i in 0..N {
//         if i < h {
//             ys.push(A[i]);
//         } else {
//             xs.push(A[i]);
//         }
//     }

//     while !ys.is_empty() {
//         assert!(xs.len() - min(xs.len(), ys.len()) <= 1);

//         let x = xs.pop().unwrap();
//         let y = ys.pop().unwrap();

//         if xs.len() > ys.len() {
//             ops.push((y, x));
//             ys.push(y - x);
//         } else {
//             ops.push((x, y));
//             xs.push(x - y);
//         }
//     }

//     assert_eq!(xs.len(), 1);

//     xs.pop().unwrap()
// }

fn main() {
    let N = read!(usize);
    let mut A = read![[i64]];
    A.sort();

    let mut ops = vec![];
    let mut xs = vec![];
    let mut ys = vec![];

    let mut h = 1;
    while h + 1 < N && A[h] < 0 {
        h += 1;
    }

    for i in 0..N {
        if i < h {
            ys.push(A[i]);
        } else {
            xs.push(A[i]);
        }
    }

    while !ys.is_empty() {
        // assert!(xs.len() - min(xs.len(), ys.len()) <= 1);

        let x = xs.pop().unwrap();
        let y = ys.pop().unwrap();

        if xs.len() > ys.len() {
            ops.push((y, x));
            ys.push(y - x);
        } else {
            ops.push((x, y));
            xs.push(x - y);
        }
    }

    assert_eq!(xs.len(), 1);
    debug_assert_eq!(*xs.last().unwrap(), brute_force(&A));

    println!("{}", xs.pop().unwrap());
    for (x, y) in ops {
        println!("{} {}", x, y);
    }
}
