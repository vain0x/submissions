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

pub fn main() {
    let w = rw::<usize>();
    let (N, C) = (w[0], w[1]);
    let mut D = vec![vec![0; C]; C];
    let mut c = vec![vec![0; N]; N];

    for X in 0..C {
        D[X] = rw::<i64>();
    }

    for y in 0..N {
        c[y] = rw::<usize>();
    }

    // (r, Y) -> sum D_*,Y
    let mut sumd = HashMap::<(usize, usize), i64>::new();

    for r in 0..3 {
        for Y in 0..C {
            let mut d = 0;

            for y in 0..N {
                for x in 0..N {
                    if ((1 + x) + (1 + y)) % 3 != r {
                        continue;
                    }

                    let X = c[y][x] - 1;
                    d += D[X][Y];
                }
            }

            sumd.insert((r, Y), d);
        }
    }
    debug!(sumd);

    let mut min_d = std::i64::MAX;
    for y1 in 0..C {
        for y2 in 0..C {
            if y1 == y2 {
                continue;
            }
            for y3 in 0..C {
                if y3 == y1 || y3 == y2 {
                    continue;
                }

                let d = sumd[&(0, y1)] + sumd[&(1, y2)] + sumd[&(2, y3)];
                min_d = min(min_d, d);
            }
        }
    }

    println!("{}", min_d);
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
