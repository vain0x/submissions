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

fn solve(A: &mut [i64]) -> i64 {
    let N = A.len();

    // 負数があるとややこしいので、最小値 m が 0 になるように全体をずらす。
    // 最終結果が「選んだ要素の数 * m」だけずれるので、最後に補正する。
    let m = *A.iter().min().unwrap();
    for i in 0..N {
        A[i] -= m;
    }
    let loss = (N / 2) as i64 * m;

    // 最終的に N/2 個選ぶ場合、A[..i] から選ぶ要素の個数は i/2-1 個以上 i/2+1 個以下でなければいけない。

    // dp[i][s][u] = x
    // (i: 0..N, s: 0..3, u: 0..2)
    // A[..i] からちょうど (i/2 + s - 1) 個選んでいる。
    // A[i-1] を選んでいるとき u=1 で、選んでいないとき u=0 とする。
    // このときの総和の最大値が x
    let mut dp = vec![vec![vec![0; 2]; 3]; N + 1];
    let mut ok = vec![vec![vec![false; 2]; 3]; N + 1];

    ok[0][1][0] = true;

    for i in 0..N {
        for s in 0..3 {
            if !ok[i][s][0] {
                continue;
            }

            // A[i] を選ぶケース
            // 遷移先が満たす条件:
            // (i/2 + s - 1) + 1 = ((i+1)/2 + t - 1)
            // t = s + 1 - ((i+1)/2 - i/2)
            let t = s + 1 - i % 2;
            if t < 3 {
                dp[i + 1][t][1] = max(dp[i + 1][t][1], dp[i][s][0] + A[i]);
                ok[i + 1][t][1] = true;
            }
        }

        // A[i] を選ばないケース
        for s in 0..3 {
            for u in 0..2 {
                if !ok[i][s][u] {
                    continue;
                }

                // (i/2 + s - 1) = ((i+1)/2 + t - 1)
                if s >= i % 2 {
                    let t = s - i % 2;
                    dp[i + 1][t][0] = max(dp[i + 1][t][0], dp[i][s][u]);
                    ok[i + 1][t][0] = true;
                }
            }
        }
    }

    let M = max(dp[N][1][0], dp[N][1][1]);

    // debug!(A, dp, m, loss, M);

    M + loss
}

fn main() {
    read!(usize);
    let mut A = read![[i64]];

    println!("{}", solve(&mut A));
}

#[cfg(test)]
mod tests {
    use super::solve;
    use std::cmp::max;

    fn brute_force(A: &[i64]) -> i64 {
        let N = A.len();
        debug_assert!(N < 20);

        let mut M = std::i64::MIN;

        for s in 0..1 << N {
            let count = (0..N).filter(|&i| (s >> i) & 1 != 0).count();
            if count != N / 2 {
                continue;
            }

            let ng = (0..N - 1).any(|i| (s >> i) & 1 != 0 && (s >> (i + 1)) & 1 != 0);
            if ng {
                continue;
            }

            let sum = (0..N).map(|i| A[i] * ((s >> i) & 1)).sum::<i64>();
            M = max(M, sum);
        }

        M
    }

    fn solve_str(s: &str) -> i64 {
        let mut A = s
            .split_whitespace()
            .map(|s| s.parse::<i64>().unwrap())
            .collect::<Vec<_>>();
        solve(&mut A)
    }

    #[test]
    fn case1() {
        assert_eq!(solve_str("1 2 3 4 5 6"), 12);
    }

    #[test]
    fn case2() {
        assert_eq!(solve_str("-1000 -100 -10 0 10"), 0);
    }

    #[test]
    fn case3() {
        assert_eq!(solve_str("1000000000 1000000000 1000000000 1000000000 1000000000 1000000000 1000000000 1000000000 1000000000 1000000000"), 5000000000);
    }

    #[test]
    fn case4() {
        assert_eq!(solve_str("18 -28 18 28 -45 90 -45 23 -53 60 28 -74 -71 35 -26 -62 49 -77 57 24 -70 -93 69 -99 59 57 -49"), 295);
    }

    #[test]
    fn random_check() {
        use std::fs;
        use std::io::{self, BufRead, Read};

        let rng = std::fs::File::open("/dev/urandom").unwrap();
        let mut r = std::io::BufReader::new(rng);
        let mut buf = [0; 2];

        let mut A = vec![];

        for N in 2..15 {
            eprintln!("N = {}", N);

            for _ in 0..100 {
                A.clear();

                for _ in 0..N {
                    r.read_exact(&mut buf).unwrap();
                    let a = i16::from_be_bytes(buf);
                    A.push(a as i64);
                }

                eprintln!("{:?}", A);
                let expected = brute_force(&A);
                let actual = solve(&mut A);
                assert_eq!(actual, expected);
            }
        }
    }
}
