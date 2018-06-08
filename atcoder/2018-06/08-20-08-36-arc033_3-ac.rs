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

fn rightmost_bit(n: usize) -> usize {
    let s = n as isize;
    (s & -s) as usize
}

fn bit_add(it: &mut Vec<i32>, index: usize, value: i32) {
    let mut j = index + 1;
    while j < it.len() {
        it[j] += value;
        j += rightmost_bit(j);
    }

    debug!(it);
}

fn bit_acc(it: &Vec<i32>, r: usize) -> i32 {
    let mut acc = 0;

    let mut j = r;
    while 0 < j && j <= it.len() {
        acc += it[j];
        j -= rightmost_bit(j);
    }

    acc
}

pub fn main() {
    let Q = rl().parse::<usize>().unwrap();
    let mut T = Vec::new();
    let mut X = Vec::new();
    let mut Y = Vec::new();
    for _ in 0..Q {
        let w = rw::<i32>();
        let (t, x) = (w[0], w[1]);
        T.push(t);
        X.push(x);

        if t == 1 {
            Y.push(x);
        }
    }

    Y.sort();
    Y.dedup();

    let mut yi_from_x = HashMap::new();
    for (yi, y) in Y.iter().cloned().enumerate() {
        yi_from_x.insert(y, yi);
    }

    let mut bit = vec![0; Y.len() + 1];

    for qi in 0..Q {
        if T[qi] == 1 {
            bit_add(&mut bit, yi_from_x[&X[qi]], 1);
        } else {
            // 0-indexed
            let w = X[qi] - 1;

            // w番目に小さい <=> 小さいものがw個以下であるような値の最大値
            let mut l = 0; // ok
            let mut r = Y.len(); // ng
            while r - l > 1 {
                let m = l + (r - l) / 2;
                if bit_acc(&bit, m) <= w {
                    l = m;
                } else {
                    r = m;
                }
            }

            println!("{}", Y[l]);

            bit_add(&mut bit, l, -1);
        }
    }
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
