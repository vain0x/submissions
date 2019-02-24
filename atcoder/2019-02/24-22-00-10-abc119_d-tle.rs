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
    buf.trim_right().to_owned()
}

// -----------------------------------------------
// Solution
// -----------------------------------------------

fn main() {
    use std::io::{BufReader, BufWriter};

    let (A, B, Q) = read!(usize, usize, usize);

    let stdin = stdin();
    let mut stdin = BufReader::new(stdin.lock());
    let mut line = String::new();
    let mut scan = move || {
        line.clear();
        stdin.read_line(&mut line).unwrap();
        line.trim_right().parse::<i64>().unwrap()
    };

    let mut S = Vec::with_capacity(A);
    for _ in 0..A {
        S.push(scan());
    }
    let mut T = Vec::with_capacity(B);
    for _ in 0..B {
        T.push(scan());
    }
    let mut X = Vec::with_capacity(Q);
    for _ in 0..Q {
        X.push(scan());
    }

    let mut Y = X
        .iter()
        .enumerate()
        .map(|(q, &x)| (x, q))
        .collect::<Vec<_>>();
    Y.sort();

    let mut D = vec![0; Q];

    // 位置 x からみてすぐ右にある神社の番号 (なければ A)
    let mut si = 0;
    let mut ti = 0;

    let mut ss = vec![];
    let mut ts = vec![];

    for yi in 0..Y.len() {
        let (x, q) = Y[yi];

        while si < A && S[si] < x {
            si += 1;
        }
        while ti < B && T[ti] < x {
            ti += 1;
        }

        ss.clear();
        if si > 0 {
            ss.push(S[si - 1]);
        }
        if si < A {
            ss.push(S[si]);
        }

        ts.clear();
        if ti > 0 {
            ts.push(T[ti - 1]);
        }
        if ti < B {
            ts.push(T[ti]);
        }

        let mut min_d = std::i64::MAX;
        for ssi in 0..ss.len() {
            for tsi in 0..ts.len() {
                let sx = ss[ssi];
                let tx = ts[tsi];
                let d = (sx - x).abs() + (tx - sx).abs() + (x - tx).abs()
                    - max((sx - x).abs(), (tx - x).abs());
                min_d = min(min_d, d);
                debug!((q, x), (si, ti), (x, sx, tx), d);
            }
        }

        D[q] = min_d;
    }

    debug!(S, T, Y, D);

    let stdout = std::io::stdout();
    let mut stdout = BufWriter::new(stdout.lock());
    for q in 0..Q {
        writeln!(stdout, "{}", D[q]).unwrap();
    }
    stdout.flush().unwrap();
}
