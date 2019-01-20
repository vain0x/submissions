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

const P: i64 = 1_000_000_007;

/// Calculates `x^n`. O(log n) time.
/// By Fermat's little theorem, `x^(-1) = pow(x, P - 2)`.
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

/// Represents an element of finite field.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Default)]
struct Finite<T>(T);

impl<T: Debug> Debug for Finite<T> {
    fn fmt(&self, formatter: &mut Formatter) -> std::fmt::Result {
        formatter.write_fmt(format_args!("{:?}", self.0))
    }
}

trait Normalize {
    fn normalize(&mut self);
}

impl<T> From<T> for Finite<T>
where
    Self: Normalize,
{
    fn from(value: T) -> Self {
        let mut it = Finite(value);
        it.normalize();
        it
    }
}

// Derive binary operation trait from the `FooAssign` impl.
// Define `x + y` as `{ let mut x = x.clone(); x += y; x }`.
macro_rules! impl_binary_op_for_finite {
    ($op_trait:ident, $op:ident, $assign_trait:ident, $assign:ident) => {
        impl<T: $op_trait<T, Output = T>> $op_trait<T> for Finite<T>
        where
            Self: Clone + $assign_trait<T>,
        {
            type Output = Self;

            fn $op(self, other: T) -> Self {
                let mut it = self.clone();
                it.$assign(other);
                it
            }
        }

        impl<T: $op_trait<T, Output = T>> $op_trait<Finite<T>> for Finite<T>
        where
            Self: Clone + $assign_trait<Finite<T>>,
        {
            type Output = Self;

            fn $op(self, other: Self) -> Self {
                let mut it = self.clone();
                it.$assign(other);
                it
            }
        }
    };
}

// Derive assign operation trait by unwrapping the right hand side.
macro_rules! impl_binary_op_assign_with_finite_for_finite {
    ($op_trait:ident, $op:ident) => {
        impl<T> $op_trait<Finite<T>> for Finite<T>
        where
            Self: $op_trait<T>,
        {
            fn $op(&mut self, other: Self) {
                self.$op(other.0);
            }
        }
    };
}

// Derive assign operation trait from impl for inner type.
macro_rules! impl_binary_op_assign_with_inner_for_finite {
    ($op_trait:ident, $op:ident) => {
        impl<T: $op_trait<T>> $op_trait<T> for Finite<T>
        where
            Self: Normalize,
        {
            fn $op(&mut self, other: T) {
                let mut other = Finite::from(other);
                other.normalize();
                (self.0).$op(other.0);
                self.normalize();
            }
        }
    };
}

impl_binary_op_for_finite! {Add, add, AddAssign, add_assign}
impl_binary_op_for_finite! {Sub, sub, SubAssign, sub_assign}
impl_binary_op_for_finite! {Mul, mul, MulAssign, mul_assign}
impl_binary_op_for_finite! {Div, div, DivAssign, div_assign}
impl_binary_op_assign_with_inner_for_finite! {AddAssign, add_assign}
impl_binary_op_assign_with_inner_for_finite! {SubAssign, sub_assign}
impl_binary_op_assign_with_inner_for_finite! {MulAssign, mul_assign}
impl_binary_op_assign_with_finite_for_finite! {AddAssign, add_assign}
impl_binary_op_assign_with_finite_for_finite! {SubAssign, sub_assign}
impl_binary_op_assign_with_finite_for_finite! {MulAssign, mul_assign}
impl_binary_op_assign_with_finite_for_finite! {DivAssign, div_assign}

impl Finite<i64> {
    fn pow(self, e: i64) -> Self {
        pow(self.0, e).into()
    }
}

impl Normalize for Finite<i64> {
    fn normalize(&mut self) {
        self.0 %= P;
        self.0 += P;
        self.0 %= P;
    }
}

impl DivAssign<i64> for Finite<i64> {
    fn div_assign(&mut self, other: i64) {
        *self *= Self::from(other).pow(P - 2);
    }
}

fn main() {
    let (N, K) = read!(usize, usize);
    let A = read![[usize]];

    // dp[i][k] = x : 子供 0..i に合計 k 個の飴をあげたとき、場合の数が x
    let mut dp = vec![Finite::from(0); K + 1];
    dp[0] = 1.into();

    for i in 0..N {
        // dp の差分 (累積和の逆)
        let mut ddp = vec![Finite::from(0); K + 1];

        for k in 0..K + 1 {
            // dp[k..t] に一様加算
            ddp[k] += dp[k];

            let t = k + A[i] + 1;
            if t <= K {
                ddp[t] -= dp[k];
            }
        }

        // 累積和
        dp[0] = ddp[0];
        for k in 1..K + 1 {
            dp[k] = dp[k - 1] + ddp[k];
        }

        // debug!(ddp);
        // debug!(dp);
    }

    println!("{}", dp[K].0)
}
