#![allow(unused_imports)]
#![allow(non_snake_case)]

use std::cell::RefCell;
use std::cmp::{max, min, Ordering};
use std::collections::*;
use std::fmt::{Debug, Formatter};
use std::io::*;
use std::mem::{replace, swap};
use std::ops::*;
use std::rc::Rc;

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
            stderr().write_fmt(format_args!("{:?}\n", entries)).unwrap();
        }
    };
}

#[allow(dead_code)]
fn rl() -> String {
    let mut buf = String::new();
    stdin().read_line(&mut buf).unwrap();
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

type Pos = (usize, usize);

#[derive(PartialEq, Eq, Clone, Debug)]
struct Route {
    cost: i64,
    vertex: Pos,
}

impl PartialOrd for Route {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        other.cost.partial_cmp(&self.cost)
    }
}

impl Ord for Route {
    fn cmp(&self, other: &Self) -> Ordering {
        other.cost.cmp(&self.cost)
    }
}

fn solve(H: usize, W: usize, K: usize, dss: &str, input: &Vec<String>) -> Option<i64> {
    let dy = vec![2, 1, 0, 1];
    let dx = vec![1, 2, 1, 0];

    let ds = dss
        .chars()
        .map(|d| match d {
            'D' => 0,
            'R' => 1,
            'U' => 2,
            'L' => 3,
            _ => unreachable!(),
        })
        .vec();

    let mut board = vec![vec!['#'; W + 2]; H + 2];
    let mut goal = (0, 0);
    let mut start = (0, 0);

    for (y, line) in input.iter().enumerate() {
        for (x, c) in line.chars().enumerate() {
            board[y + 1][x + 1] = c;

            match c {
                'S' => {
                    start = (1 + y, 1 + x);
                }
                'G' => {
                    goal = (1 + y, 1 + x);
                }
                _ => {}
            }
        }
    }

    let (sy, sx) = start;
    let (gy, gx) = goal;

    // wait[d][k] = dt
    // <=> 現在時刻の剰余が k のとき、次に d 方向に遷移するまでに待つべき時間の最小が dt
    let mut wait: Vec<Vec<Option<i64>>> = vec![vec![None; K]; 4];
    {
        for d in 0..4 {
            for _ in 0..2 {
                for k in (0..K).rev() {
                    wait[d][k] = if ds[k] == d {
                        Some(0)
                    } else {
                        wait[d][(k + 1) % K].map(|x| x + 1)
                    };
                }
            }
        }
    }

    let mut T = vec![vec![std::i64::MAX; W + 2]; H + 2];
    T[sy][sx] = 0;

    let mut heap = BinaryHeap::new();
    heap.push(Route {
        cost: 0,
        vertex: (sy, sx),
    });

    while let Some(Route {
        cost: vt,
        vertex: (vy, vx),
    }) = heap.pop()
    {
        for d in 0..4 {
            let (wy, wx) = (vy + dy[d] - 1, vx + dx[d] - 1);
            if board[wy][wx] == '#' {
                continue;
            }

            if let Some(dt) = wait[d][(vt % K as i64) as usize] {
                let wt = vt + dt + 1;

                if T[wy][wx] > wt {
                    T[wy][wx] = wt;
                    heap.push(Route {
                        cost: wt,
                        vertex: (wy, wx),
                    });
                }
            }
        }
    }

    let t = T[gy][gx];
    if t < std::i64::MAX {
        Some(t)
    } else {
        None
    }
}

pub fn main() {
    let (H, W, K) = read!(usize, usize, usize);
    let ds = rl();
    let input = read![String; H];

    if let Some(t) = solve(H, W, K, &ds, &input) {
        println!("{}", t);
    } else {
        println!("-1");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn input(s: &str) -> Vec<String> {
        s.lines().map(|l| l.to_owned()).vec()
    }

    #[test]
    fn test1() {
        assert_eq!(
            solve(
                2,
                3,
                5,
                "UDRRL",
                &input(
                    r#"S..
.#G"#
                )
            ),
            Some(7)
        );
    }

    #[test]
    fn test2() {
        let input = input(
            r#"S..
...
.G."#,
        );
        assert_eq!(solve(3, 3, 5, "UDUDD", &input), None);
    }

    #[test]
    fn test3() {
        let input = input(
            r#"S..
.#.
..G"#,
        );
        assert_eq!(solve(3, 3, 5, "UDLRD", &input), Some(12));
    }
}
