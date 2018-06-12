#![allow(unused_imports)]
#![allow(non_snake_case)]

use std::cmp::{max, min, Ordering};
use std::collections::*;
use std::io::*;
use std::ops::*;
use std::*;

// -----------------------------------------------
// Framework
// -----------------------------------------------

#[allow(unused)]
fn rl() -> String {
    let mut buf = String::new();
    io::stdin().read_line(&mut buf).unwrap();
    buf.trim_right().to_owned()
}

#[allow(unused)]
fn rw<T>() -> Vec<T>
where
    T: std::str::FromStr,
    T::Err: std::fmt::Debug,
{
    let mut buf = String::new();
    io::stdin().read_line(&mut buf).unwrap();
    buf.split_whitespace()
        .map(|word| T::from_str(word).unwrap())
        .collect()
}

trait IteratorExt: Iterator + Sized {
    fn vec(self) -> Vec<Self::Item> {
        self.collect()
    }
}

impl<T: Iterator> IteratorExt for T {}

#[allow(unused)]
macro_rules! debug {
    ($($arg:expr),*) => {
        #[cfg(debug_assertions)]
        {
            let entries = &[
                $((
                    &stringify!($arg).to_string() as &fmt::Debug,
                    &($arg) as &fmt::Debug,
                )),*
            ];
            eprintln!("{:?}", DebugMap(entries));
        }
    };
}

#[allow(unused)]
struct DebugMap<'a>(&'a [(&'a fmt::Debug, &'a fmt::Debug)]);

impl<'a> std::fmt::Debug for DebugMap<'a> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let mut m = fmt.debug_map();
        for &(key, value) in self.0.iter() {
            m.entry(key, value);
        }
        m.finish()
    }
}

// -----------------------------------------------
// Polyfill
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

#[allow(unused)]
macro_rules! eprintln {
    ($($arg:expr),*) => { _eprintln(format_args!($($arg),*)) }
}

fn _eprintln(args: fmt::Arguments) {
    let err = std::io::stderr();
    let mut err = err.lock();
    err.write_fmt(args).unwrap();
    err.write(b"\n").unwrap();
}

// -----------------------------------------------
// Solution
// -----------------------------------------------

fn roots_dfs(v: usize, root: usize, N: usize, g: &Vec<Vec<(usize, usize)>>, done: &mut Vec<usize>) {
    if done[v] == root {
        return;
    }
    done[v] = root;

    for &(_, w) in g[v].iter() {
        roots_dfs(w, root, N, g, done);
    }
}

fn roots(N: usize, g: &Vec<Vec<(usize, usize)>>) -> Vec<usize> {
    let mut roots = vec![N; N];
    for v in 0..N {
        roots_dfs(v, v, N, g, &mut roots);
    }
    roots
}

pub fn main() {
    let w = rw::<usize>();
    let (N, K, L) = (w[0], w[1], w[2]);

    let mut roads = vec![Vec::new(); N];
    for _ in 0..K {
        let w = rw::<usize>();
        let (p, q) = (w[0] - 1, w[1] - 1);
        roads[p].push((p, q));
        roads[q].push((q, p));
    }

    let mut rails = vec![Vec::new(); N];
    for _ in 0..L {
        let w = rw::<usize>();
        let (p, q) = (w[0] - 1, w[1] - 1);
        rails[p].push((p, q));
        rails[q].push((q, p));
    }

    let rail_roots = roots(N, &rails);
    let road_roots = roots(N, &roads);
    let mut map = HashMap::new();
    for v in 0..N {
        *map.entry((rail_roots[v], road_roots[v])).or_insert(0) += 1;
    }

    let zs = (0..N)
        .map(|v| map[&(rail_roots[v], road_roots[v])].to_string())
        .collect::<Vec<String>>();
    println!("{}", zs.join(" "));
    return;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ok() {
        assert_eq!(7, 1 + 2 * 3);
    }
}
