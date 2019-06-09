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

pub fn pow(x: i64, n: i64) -> i64 {
    let (mut x, mut y, mut n) = (x % P, 1_i64, n);
    while n > 0 {
        if n % 2 != 0 {
            y = (y * x) % P;
            n -= 1;
        }

        x = (x * x) % P;
        n /= 2;
    }
    y
}

/// P で割った余り
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Default)]
struct ModInt(i64);

impl ModInt {
    fn pow(self, e: i64) -> Self {
        pow(self.0, e).into()
    }
}

impl Debug for ModInt {
    fn fmt(&self, formatter: &mut Formatter) -> std::fmt::Result {
        formatter.write_fmt(format_args!("{:?}", self.0))
    }
}

impl Display for ModInt {
    fn fmt(&self, formatter: &mut Formatter) -> std::fmt::Result {
        formatter.write_fmt(format_args!("{}", self.0))
    }
}

impl From<i64> for ModInt {
    fn from(value: i64) -> Self {
        ModInt((value % P + P) % P)
    }
}

// Generate binary operation traits.
macro_rules! impl_binary_op_for_modint {
    ($op_trait:ident, $op:ident, $assign_trait:ident, $assign:ident $(, $f:ident)*) => {
        $(impl $op_trait<ModInt> for ModInt {
            type Output = Self;

            fn $op(self, other: Self) -> Self {
                ModInt::from((self.0).$f(other.0))
            }
        })*

        impl $op_trait<i64> for ModInt {
            type Output = Self;

            fn $op(self, other: i64) -> Self {
                self.$op(ModInt::from(other))
            }
        }

        impl $assign_trait<ModInt> for ModInt {
            fn $assign(&mut self, other: Self) {
                *self = self.$op(other)
            }
        }

        impl $assign_trait<i64> for ModInt {
            fn $assign(&mut self, other: i64) {
                *self = self.$op(other)
            }
        }
    };
}

impl_binary_op_for_modint! {Add, add, AddAssign, add_assign, add}
impl_binary_op_for_modint! {Sub, sub, SubAssign, sub_assign, sub}
impl_binary_op_for_modint! {Mul, mul, MulAssign, mul_assign, mul}
impl_binary_op_for_modint! {Div, div, DivAssign, div_assign}

impl Div<ModInt> for ModInt {
    type Output = ModInt;

    fn div(self, other: Self) -> Self {
        self * other.pow(P - 2)
    }
}

// -----------------------------------------------
// Solution
// -----------------------------------------------

const P: i64 = 1_000_000_007;

// 3**19 % 1000000007

fn main() {
    let L = rl().chars().collect::<Vec<_>>();
    let N = L.len();

    // dp[i][eq] = n:
    // 上から i 桁を決めたときの場合の数
    // それらが L と一致しているなら eq=1
    let mut dp = vec![vec![ModInt::default(); 2]; N + 1];

    dp[0][1] = 1.into();

    for i in 0..N {
        if L[i] == '0' {
            // 0
            dp[i + 1][1] += dp[i][1];
        } else {
            // 0
            dp[i + 1][0] += dp[i][1];

            // 1
            dp[i + 1][1] += dp[i][1] * 2;
        }

        // 0, 1
        dp[i + 1][0] += dp[i][0] * 3;
    }

    println!("{}", dp[N][0] + dp[N][1])
}
