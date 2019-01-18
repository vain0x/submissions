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

struct Solver {
    N: usize,
    x: usize,
    y: usize,
    z: usize,
    dp: Vec<Vec<Vec<Option<f64>>>>,
}

impl Solver {
    fn calc(&mut self, x: usize, y: usize, z: usize) -> f64 {
        let s = x + y + z;
        if s == 0 {
            return 0.0;
        }

        if let Some(e) = self.dp[x][y][z] {
            return e;
        }

        let mut e = 0.0_f64;
        e += self.N as f64;
        if x >= 1 {
            e += x as f64 * self.calc(x - 1, y, z);
        }
        if y >= 1 {
            e += y as f64 * self.calc(x + 1, y - 1, z);
        }
        if z >= 1 {
            e += z as f64 * self.calc(x, y + 1, z - 1);
        }
        e /= s as f64;

        self.dp[x][y][z] = Some(e);
        e
    }

    fn solve(&mut self) {
        let x = self.x;
        let y = self.y;
        let z = self.z;
        let e = self.calc(x, y, z);
        println!("{}", e)
    }
}

fn main() {
    let N = read!(usize);
    let A = read![[usize]];

    let mut x = 0;
    let mut y = 0;
    let mut z = 0;
    for &a in &A {
        match a {
            1 => x += 1,
            2 => y += 1,
            3 => z += 1,
            _ => {}
        }
    }

    let dp = vec![vec![vec![None; N + 1]; N + 1]; N + 1];
    Solver {
        N: N,
        x: x,
        y: y,
        z: z,
        dp: dp,
    }
    .solve()
}
