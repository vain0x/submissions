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

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Event {
    Push(i64),
    Pop(i64),
    Mark,
    Query,
}

fn main() {
    let (N, mut W, C) = read!(usize, i64, i64);
    W += 1;

    let T = read![i64, i64, i64; N];

    let mut events = vec![];
    let mut checkpoints = vec![0, W - C];

    for i in 0..N {
        let (mut l, r, p) = T[i];
        l += 1;

        events.push((l, Event::Push(p)));
        events.push((r, Event::Pop(p)));

        checkpoints.push(l - C);
        checkpoints.push(l);
        checkpoints.push(r - C);
        checkpoints.push(r);
        checkpoints.push(r + C);
    }

    checkpoints.sort();
    checkpoints.dedup();
    for t in checkpoints {
        events.push((t, Event::Mark));
        events.push((t + C, Event::Query));
    }

    events.sort();

    debug!(events);

    let mut pops = BTreeMap::new();
    let mut current_push = 0;
    let mut current_pop = 0;
    let mut min_cost = std::i64::MAX;

    for (t, event) in events {
        debug!((t, &event));

        match event {
            Event::Push(p) => {
                current_push += p;
            }
            Event::Pop(p) => {
                current_pop += p;
            }
            Event::Mark => {
                let e = pops.insert(t, current_pop);
                debug_assert!(e.is_none());
            }
            Event::Query => {
                if t - C < 0 || t > W {
                    continue;
                }

                let left_pop = pops[&(t - C)];
                let cost = current_push - left_pop;
                debug!((t, cost));
                min_cost = min(min_cost, cost);
            }
        }
    }

    debug!(pops, current_push, current_pop);

    println!("{}", min_cost)
}
