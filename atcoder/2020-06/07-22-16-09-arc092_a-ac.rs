//! Framework <https://github.com/vain0x/procon>

#![allow(non_snake_case)]
#![allow(unused_imports)]

use std::cell::RefCell;
use std::cmp::{max, min, Ordering};
use std::collections::*;
use std::fmt::{Debug, Display, Formatter, Write as FmtWrite};
use std::io::{stderr, stdin, BufRead, Write};

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

// ###############################################

/// `adjacent_fn(u) = k`: 頂点 u から出る辺の個数 k
/// `edge_fn(u, i) = (v, capacity)`: 頂点 u から出る i 番目の辺の先の頂点が v 、許容量が capacity
pub fn ford_fulkerson<
    AdjacentFn: Fn(usize) -> usize,
    EdgeFn: Fn(usize, usize) -> (usize, isize),
>(
    s: usize,
    t: usize,
    n: usize,
    adjacent_fn: AdjacentFn,
    edge_fn: EdgeFn,
) -> isize {
    type VertexId = usize;
    type EdgeId = usize;
    type Flow = isize;

    struct EdgeData {
        v: VertexId,
        capacity: Flow,
        rev: EdgeId,
    }

    struct FordFulkerson {
        t: VertexId,
        g: Vec<Vec<EdgeId>>,
        edges: Vec<EdgeData>,
        done: Vec<bool>,
    }

    const INF: Flow = 1 << 60;

    fn dfs(u: VertexId, flow: &mut Flow, ff: &mut FordFulkerson) -> bool {
        if u == ff.t {
            return true;
        }

        ff.done[u] = true;

        for i in 0..ff.g[u].len() {
            let ei = ff.g[u][i];
            let EdgeData {
                v, capacity, rev, ..
            } = ff.edges[ei];

            if ff.done[v] || capacity <= 0 {
                continue;
            }

            let mut d = std::cmp::min(*flow, capacity);
            if !dfs(v, &mut d, ff) {
                continue;
            }

            ff.edges[ei].capacity -= d;
            ff.edges[rev].capacity += d;
            *flow = d;
            return true;
        }

        false
    }

    let mut ff = FordFulkerson {
        t: t,
        g: vec![vec![]; n],
        edges: vec![],
        done: vec![],
    };

    // グラフを構築する。
    for u in 0..n {
        let k = adjacent_fn(u);
        for i in 0..k {
            let (v, capacity) = edge_fn(u, i);

            let ei = ff.edges.len();
            ff.edges.push(EdgeData {
                v: v,
                capacity: capacity,
                rev: std::usize::MAX,
            });

            let rev = ff.edges.len();
            ff.edges.push(EdgeData {
                v: u,
                capacity: 0,
                rev: ei,
            });

            ff.edges[ei].rev = rev;

            ff.g[u].push(ei);
            ff.g[v].push(rev);
        }
    }

    // DFS.
    let mut total_flow = 0;
    loop {
        ff.done.clear();
        ff.done.resize(n, false);

        let mut flow = INF;
        if dfs(s, &mut flow, &mut ff) {
            total_flow += flow;
            continue;
        }

        return total_flow;
    }
}

fn main() {
    let N = read!(usize);
    let red = read![u32, u32; N];
    let blue = read![u32, u32; N];

    // i 番目の赤: i
    // j 番目の青: i + N
    // 始点 (source)
    let s = N * 2;
    // 終点 (sink)
    let t = N * 2 + 1;
    // 始点 → 赤 → 青 → 終点 という方向のグラフ
    let mut g = vec![vec![]; N * 2 + 2];

    for ri in 0..N {
        let (rx, ry) = red[ri];
        g[s].push((ri, 1));

        for bi in 0..N {
            let (bx, by) = blue[bi];

            if rx < bx && ry < by {
                g[ri].push((bi + N, 1));
            }
        }
    }

    for bi in 0..N {
        g[bi + N].push((t, 1));
    }

    let flow = ford_fulkerson(s, t, g.len(), |u| g[u].len(), |u, i| g[u][i]);
    println!("{}", flow)
}
