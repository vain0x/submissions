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

// 鬼(青木)の初期位置を根とする根つき木を計算
// 兎(高橋)は最も深いリーフに逃げ込みたい
// リーフを深い順に調べる
// 兎の初期位置とリーフのLCAを求める
// 兎と鬼がLCAを目指すとき鬼が先に到達するならダメ
// 兎が先に到達するなら兎がそのリーフに潜って追いつめられるまでをシミュレーションして解とする

struct Solver {
    N: usize,
    // 兎の初期位置
    U: usize,
    // 鬼の初期位置
    V: usize,
    G: Vec<Vec<usize>>,
    // 深さ、端点
    leaves: Vec<(usize, usize)>,
    // a[u][h]: u の 2^h 世代上の親要素
    ancestors: Vec<Vec<usize>>,
    depths: Vec<usize>,
}

impl Solver {
    fn dfs(&mut self, u: usize, parent: usize, depth: usize) {
        self.depths[u] = depth;
        debug!((u, parent, depth));

        let mut h = 0;
        // u の 2^h 世代上
        let mut a = parent;
        while a < self.N {
            self.ancestors[u].push(a);
            debug!((u, h, a));

            if h >= self.ancestors[a].len() {
                break;
            }

            // u の 2^(h+1) 世代上は a の 2^h 世代上
            a = self.ancestors[a][h];
            h += 1;
        }

        let mut is_leaf = true;

        for i in 0..self.G[u].len() {
            let v = self.G[u][i];
            if v == parent {
                continue;
            }

            is_leaf = false;
            self.dfs(v, u, depth + 1);
        }

        if is_leaf {
            debug!(("leaf", u));
            self.leaves.push((depth, u));
        }
    }

    fn lca(&self, mut u: usize, mut v: usize) -> usize {
        debug!(("lca", u, v));

        while u != v {
            let ud = self.depths[u];
            let vd = self.depths[v];

            if ud == vd {
                let mut h = 0;
                while h + 1 < self.ancestors[u].len()
                    && self.ancestors[u][h + 1] != self.ancestors[v][h + 1]
                {
                    h += 1;
                }

                debug!(("ud==vd", u, h, self.ancestors[u][h]));
                debug!(("ud==vd", v, h, self.ancestors[v][h]));
                u = self.ancestors[u][h];
                v = self.ancestors[v][h];
                continue;
            }

            if ud > vd {
                std::mem::swap(&mut u, &mut v);
            }
            assert!(ud < vd);

            let mut h = 0;
            while h + 1 < self.ancestors[v].len() && (1 << (h + 1)) < vd - ud {
                h += 1;
            }

            debug!(("ud<vd", v, h, self.ancestors[v][h]));
            v = self.ancestors[v][h];
        }

        debug!(("  -->", u));
        u
    }

    fn solve(&mut self) -> usize {
        let (V, N) = (self.V, self.N);
        self.dfs(V, N, 0);

        self.leaves.sort();
        self.leaves.reverse();

        let ud = self.depths[self.U];

        let mut escape = 0;
        let mut hesitate = 0;

        for &(yd, y) in self.leaves.iter() {
            let x = self.lca(self.U, y);
            let xd = self.depths[x];

            assert!(ud >= xd);
            assert!(yd >= xd);

            if x == self.U {
                // 兎が枝から出ずに右往左往するケース
                // 兎が y に至る移動回数
                let s = yd - xd;

                // 鬼の移動回数
                let mut t = s - min(s, 1);

                // 兎が初めから y にいたのでなければ鬼が先手
                if s != 0 {
                    t += 1;
                }

                // 鬼が兎を追いつめるまでの鬼の移動回数を加算
                t += xd / 2;

                debug!(("rabbit", yd, y, xd, x, s, t));
                hesitate = max(hesitate, t);
            }

            // ud - xd : 兎の x に至る移動回数
            // xd : 鬼の x に至る移動回数
            if self.lca(x, y) != x && ud - xd >= xd {
                // 鬼が先に x に到達するので兎は y にいけない
                debug!(("ogre", yd, y, xd, x, ud - xd, xd));
                continue;
            }

            // 兎が y に至る移動回数
            let s = (ud - xd) + (yd - xd);

            // 鬼の移動回数
            let mut t = s - min(s, 1);

            // 鬼が兎を追いつめるまでの鬼の移動回数を加算
            t += (xd - (ud - xd) + 1) / 2;

            // 兎が初めから y にいるときは鬼が先手
            if s == 0 && t >= 1 {
                t -= 1;
            }

            debug!(("rabbit", yd, y, xd, x, s, t));
            escape = max(escape, t);
        }

        debug!("???");
        max(escape, hesitate)
    }
}

fn main() {
    let (N, U, V) = read!(usize, usize, usize);
    let T = read![usize, usize; N - 1];

    let mut G = vec![vec![]; N];
    for (u, v) in T {
        G[u - 1].push(v - 1);
        G[v - 1].push(u - 1)
    }

    let k = Solver {
        N: N,
        U: U - 1,
        V: V - 1,
        G: G,
        leaves: vec![],
        ancestors: vec![vec![]; N],
        depths: vec![0; N],
    }
    .solve();

    println!("{}", k)
}
