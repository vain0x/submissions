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

6
1 2
1 3
1 4
1 5
1 6

*/

fn rerooting<
    T: Clone,
    E: IntoIterator<Item = (usize, usize)>,
    F: FnMut(T, T) -> T,
    G: FnMut(T, usize) -> T,
>(
    node_count: usize,
    edges: E,
    identity: T,
    mut operate: F,
    mut operate_node: G,
) -> Vec<T> {
    const NO_PARENT: usize = std::usize::MAX;

    let mut adjacents = vec![vec![]; node_count];
    let mut index_for_adjacents = vec![vec![]; node_count];

    for (u, v) in edges {
        index_for_adjacents[u].push(adjacents[v].len());
        index_for_adjacents[v].push(adjacents[u].len());
        adjacents[u].push(v);
        adjacents[v].push(u);
    }

    if node_count == 0 {
        return vec![];
    }

    if node_count == 1 {
        return vec![operate_node(identity, 0)];
    }

    let mut parents = vec![0; node_count];
    let mut order = vec![0; node_count];

    // initialize ordered tree
    {
        let mut index = 0;
        let mut stack = vec![0];
        parents[0] = NO_PARENT;

        while let Some(node) = stack.pop() {
            order[index] = node;
            index += 1;

            for i in 0..adjacents[node].len() {
                let adjacent = adjacents[node][i];
                if adjacent == parents[node] {
                    continue;
                }

                stack.push(adjacent);
                parents[adjacent] = node;
            }
        }
    }

    let mut dp = (0..node_count)
        .map(|i| vec![identity.clone(); adjacents[i].len()])
        .collect::<Vec<_>>();

    // from leaf
    for i in (1..node_count).rev() {
        let node = order[i];
        let parent = parents[node];

        let mut accum = identity.clone();
        let mut parent_index = NO_PARENT;

        for j in 0..adjacents[node].len() {
            if adjacents[node][j] == parent {
                parent_index = j;
                continue;
            }

            accum = operate(accum, dp[node][j].clone());
        }

        dp[parent][index_for_adjacents[node][parent_index]] = operate_node(accum, node);
    }

    let mut res = vec![identity.clone(); node_count];
    let mut accums_from_tail = vec![];

    // to leaf
    for i in 0..node_count {
        let node = order[i];
        let deg = adjacents[node].len();
        let mut accum = identity.clone();

        accums_from_tail.clear();
        accums_from_tail.extend(std::iter::repeat(identity.clone()).take(deg));

        for j in (1..deg).rev() {
            accums_from_tail[j - 1] = operate(accums_from_tail[j].clone(), dp[node][j].clone());
        }

        for j in 0..deg {
            dp[adjacents[node][j]][index_for_adjacents[node][j]] =
                operate_node(operate(accum.clone(), accums_from_tail[j].clone()), node);
            accum = operate(accum, dp[node][j].clone());
        }

        res[node] = operate_node(accum, node);
    }

    res
}

const P: i64 = 1_000_000_007;

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

/// 部分木に対して持つ計算結果
#[derive(Clone, Copy)]
struct X {
    /// 場合の数
    combo: i64,
    /// 頂点数
    size: usize,
    /// (size!)^(-1)
    divisor: i64,
}

impl X {
    fn empty() -> X {
        X {
            combo: 1,
            size: 0,
            divisor: 1,
        }
    }

    fn append(self, other: X) -> X {
        X {
            combo: self.combo * other.combo % P,
            size: self.size + other.size,
            divisor: self.divisor * other.divisor % P,
        }
    }
}

fn main() {
    let N = read!(usize);
    let E = read![usize, usize; N - 1];

    let M = N + 1;
    let mut fact = vec![0; M];
    fact[0] = 1;
    for i in 1..M {
        fact[i] = fact[i - 1] * (i as i64) % P;
    }

    let mut fact_inv = vec![0; M];
    fact_inv[M - 1] = pow(fact[M - 1], P - 2);
    for i in (0..M - 1).rev() {
        fact_inv[i] = fact_inv[i + 1] * ((i + 1) as i64) % P;
    }

    let operate_node = |x: X, _u: usize| -> X {
        // x: 各部分木に対する operate_node の結果を X::append で合成したもの
        X {
            combo: fact[x.size] * x.combo % P * x.divisor % P,
            size: x.size + 1,
            divisor: fact_inv[x.size + 1],
        }
    };

    let res = rerooting(
        N,
        E.into_iter().map(|(u, v)| (u - 1, v - 1)),
        X::empty(),
        X::append,
        operate_node,
    );

    for i in 0..N {
        println!("{}", res[i].combo);
    }
}
