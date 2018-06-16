#![allow(unused_imports)]
#![allow(non_snake_case)]

use std::cmp::{max, min, Ordering};
use std::collections::*;
use std::fmt::{Debug, Formatter};
use std::io::*;
use std::ops::*;
use std::*;

// -----------------------------------------------
// Framework
// -----------------------------------------------

#[allow(unused_macros)]
macro_rules! read {
    ([$t:ty] ; $n:expr) =>
        ((0..$n).map(|_| read!([$t])).collect::<Vec<_>>());
    ($($t:ty),+ ; $n:expr) =>
        ((0..$n).map(|_| read!($($t),+)).collect::<Vec<_>>());
    ([$t:ty]) =>
        (rl().split_whitespace().map(|w| w.parse().unwrap()).collect::<Vec<$t>>());
    ($t:ty) =>
        (rl().parse::<$t>().unwrap());
    ($($t:ty),*) => {{
        let buf = rl();
        let mut w = buf.split_whitespace();
        ($(w.next().unwrap().parse::<$t>().unwrap()),*)
    }};
}

#[allow(unused_macros)]
macro_rules! debug {
    ($($arg:expr),*) => {
        #[cfg(debug_assertions)]
        {
            let entries = [$(&stringify!([$arg]:), &$arg as &Debug),*];
            stderr().write_fmt(format_args!("{:#?}\n", entries)).unwrap();
        }
    };
}

#[allow(dead_code)]
fn rl() -> String {
    let mut buf = String::new();
    io::stdin().read_line(&mut buf).unwrap();
    buf.trim_right().to_owned()
}

trait IteratorExt: Iterator + Sized {
    fn vec(self) -> Vec<Self::Item> {
        self.collect()
    }
}

impl<T: Iterator> IteratorExt for T {}

// -----------------------------------------------
// Solution
// -----------------------------------------------

type V = (i64, i64, i64);

fn dot((sa, sb, sc): V, (x, y, z): V) -> V {
    (sa * x, sb * y, sc * z)
}

// fn score(S: V, T: V) -> i64 {
//     sum(dot(S, T))
// }

// fn sum((x, y, z): V) -> i64 {
//     x + y + z
// }

// extern crate rand;
// use rand::Rng;

// fn recurse<X, Y>(x: X, f: &Fn(X, &Fn(X) -> Y) -> Y) -> Y {
//     f(x, &|x: X| recurse(x, &f))
// }

// fn check() {
//     let mut rng = rand::thread_rng();
//     loop {
//         let N = rng.gen_range(2, 15);
//         let M = rng.gen_range(2, N + 1);
//         let mut T = Vec::new();
//         for _ in 0..N {
//             let (x, y, z) = rng.gen::<(i32, i32, i32)>();
//             T.push((x as i64, y as i64, z as i64));
//         }

//         let E = bruteforce(N, M, &T);
//         let A = solve(N, M, &T);
//         if A != E {
//             debug!(E, A, N, M, T);
//             break;
//         }
//     }
// }

fn add((l1, l2, l3): V, (r1, r2, r3): V) -> V {
    (l1 + r1, l2 + r2, l3 + r3)
}

fn abs_sum((x, y, z): V) -> i64 {
    x.abs() + y.abs() + z.abs()
}

// pub fn bruteforce(N: usize, M: usize, T: &Vec<V>) -> i64 {
//     let max_score = std::cell::RefCell::new(0);

//     recurse((0, 0, (0, 0, 0)), &|(i, m, total): (usize, usize, V),
//                                  go| {
//         if m == M {
//             let (x, y, z) = total;
//             let ms = max(*max_score.borrow(), x.abs() + y.abs() + z.abs());
//             *max_score.borrow_mut() = ms;
//             return;
//         }

//         if i == N {
//             return;
//         }

//         go((i + 1, m, total));
//         go((i + 1, m + 1, add(total, T[i])));
//     });

//     let c = max_score.borrow();
//     *c
// }

fn solve(N: usize, M: usize, t: &Vec<V>) -> i64 {
    let mut max_score = 0;

    for &sa in &[1, -1] {
        for &sb in &[1, -1] {
            for &sc in &[1, -1] {
                let mut dp = vec![(0, 0, 0); M + 1];
                let S = (sa, sb, sc);

                for i in 0..N {
                    for w in (0..M).rev() {
                        let l = dp[w + 1];
                        let r = add(dp[w], dot(S, t[i]));
                        if abs_sum(l) < abs_sum(r) {
                            dp[w + 1] = r;
                        }
                    }
                }

                max_score = max(max_score, abs_sum(dp[M]));
            }
        }
    }
    max_score
}

// fn trial() {
//     let N = 3;
//     let M = 3;
//     let T = vec![
//         (1515506146, 1219647515, -722727681),
//         (2003374755, -2023068732, -1146452171),
//         (-1310420398, 800581395, 152109158),
//     ];

//     assert_eq!(3928371019, solve(N, M, &T));
// }

pub fn main() {
    // (sa, sb, sc) : a,b,cにかかる係数
    // dp[v] = (a, b, c) : v個選んでいて、S dot (a, b, c) の値が最大になる選び方

    // let v = vec![
    //     (1515506146, 1219647515, -722727681),
    //     (2003374755, -2023068732, -1146452171),
    //     (-1310420398, 800581395, 152109158),
    // ];

    // let (x, y, z) = v.into_iter().fold((0, 0, 0), |l, r| add(l, r));
    // debug!(x, y, z, x.abs() + y.abs() + z.abs());

    // check();
    // debug!(trial());

    let (N, M) = read!(usize, usize);
    let t = read![i64, i64, i64; N];
    println!("{}", solve(N, M, &t));

    return;
}
