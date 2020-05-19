//! Framework <https://github.com/vain0x/procon>

#![allow(non_snake_case)]
#![allow(unused_imports)]

use std::cmp::{max, min, Ordering};
use std::collections::*;
use std::fmt::{Debug, Display, Formatter, Write as FmtWrite};
use std::io::{stderr, stdin, BufRead, Write};

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

    #[allow(deprecated)]
    buf.trim_right().to_owned()
}

// -----------------------------------------------
// Solution
// -----------------------------------------------

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Rev<T>(pub T);

impl<T: PartialOrd> PartialOrd for Rev<T> {
    fn partial_cmp(&self, other: &Rev<T>) -> Option<Ordering> {
        other.0.partial_cmp(&self.0)
    }
}

impl<T: Ord> Ord for Rev<T> {
    fn cmp(&self, other: &Rev<T>) -> Ordering {
        other.0.cmp(&self.0)
    }
}

const INF: i64 = 1_i64 << 60;

struct Solve {
    H: usize,
    W: usize,
    A: Vec<Vec<i64>>,
    q: BinaryHeap<(Rev<i64>, usize, usize)>,
}

static DYX: [(isize, isize); 4] = [(1, 0), (0, 1), (-1_isize, 0), (0, -1_isize)];

impl Solve {
    fn solve(&mut self) {
        let (H, W) = (self.H, self.W);

        let dist_lb = self.dijkstra(H - 1, 0);
        let dist_rb = self.dijkstra(H - 1, W - 1);
        let dist_rt = self.dijkstra(0, W - 1);

        let mut m = INF;
        for y in 0..H {
            for x in 0..W {
                let d = dist_lb[y][x] + dist_rb[y][x] + dist_rt[y][x] - self.A[y][x] * 2;
                m = min(m, d);
            }
        }
        println!("{}", m);
    }

    fn dijkstra(&mut self, y0: usize, x0: usize) -> Vec<Vec<i64>> {
        let (H, W) = (self.H, self.W);

        let mut dist = vec![vec![INF; W]; H];
        dist[y0][x0] = 0;

        let q = &mut self.q;
        q.clear();
        q.push((Rev(0), y0, x0));

        while let Some((Rev(d), y, x)) = q.pop() {
            for &(dy, dx) in &DYX {
                let y2 = y as isize + dy;
                let x2 = x as isize + dx;
                if 0 <= y2 && y2 < H as isize && 0 <= x2 && x2 < W as isize {
                    let (y2, x2) = (y2 as usize, x2 as usize);
                    let d2 = d + self.A[y2][x2];
                    if dist[y2][x2] > d2 {
                        dist[y2][x2] = d2;
                        q.push((Rev(d2), y2, x2));
                    }
                }
            }
        }

        dist
    }
}

fn main() {
    let (H, W) = read!(usize, usize);
    let A = read![[i64]; H];

    Solve {
        H: H,
        W: W,
        A: A,
        q: BinaryHeap::new(),
    }
    .solve();
}
