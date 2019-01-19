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
use std::fmt::{Debug, Formatter, Write as FmtWrite};
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

#[derive(Clone, Copy)]
enum Op {
    Min,
    Max,
}

impl Op {
    fn append(self, l: i64, r: i64) -> i64 {
        match self {
            Op::Min => min(l, r),
            Op::Max => max(l, r),
        }
    }
}

struct Extreme {
    op: Op,
    value: Option<i64>,
}

impl Extreme {
    fn from_op(op: Op) -> Self {
        Extreme {
            op: op,
            value: None,
        }
    }

    fn append(self, r: i64) -> Self {
        let value = match self.value {
            Some(l) => Some(self.op.append(l, r)),
            None => Some(r),
        };
        Extreme {
            value: value,
            ..self
        }
    }
}

struct Solver {
    A: Vec<i64>,
    dp: Vec<Vec<Option<i64>>>,
}

impl Solver {
    fn calc(&mut self, l: usize, r: usize) -> i64 {
        if l == r {
            return 0_i64;
        }

        if let Some(z) = self.dp[l][r] {
            return z;
        }

        let N = self.A.len();
        let t = (N - (r - l)) % 2;
        let (extreme, sign) = if t % 2 == 0 {
            (Extreme::from_op(Op::Max), 1)
        } else {
            (Extreme::from_op(Op::Min), -1)
        };

        let mut z = extreme;

        if l + 1 < N {
            z = z.append(self.calc(l + 1, r) + sign * self.A[l]);
        }
        z = z.append(self.calc(l, r - 1) + sign * self.A[r - 1]);

        let z = z.value.unwrap();
        self.dp[l][r] = Some(z);
        z
    }

    fn solve(&mut self) {
        let N = self.A.len();
        println!("{}", self.calc(0, N));
    }
}

fn main() {
    let _ = read!(usize);
    let A = read![[i64]];

    let N = A.len();
    let dp = vec![vec![None; N + 1]; N + 1];

    Solver { A: A, dp: dp }.solve()
}
