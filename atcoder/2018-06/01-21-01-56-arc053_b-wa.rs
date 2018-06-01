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
    let S = rl();
    let mut set = HashSet::new();
    let mut n = 0;
    let mut p = 0;
    for c in S.chars() {
        n += 1;
        if set.contains(&c) {
            set.remove(&c);
            p += 1;
        } else {
            set.insert(c);
        }
    }
    let q = n - 2 * p;
    let x = {
        let mut xl = 1; // ok
        let mut xr = n + 1; // ng
        while xr - xl >= 2 {
            let x = xl + (xr - xl) / 2;

            // 長さの最小値を x 以上にできる？
            let ok = if q == 0 {
                // X=n まである
                x <= n
            } else if x % 2 == 0 {
                // q+1 個の部分列に分割して >=x を達成できる？
                // いずれかの長さがちょうどx、残りがx+1になるようにするときの最小の要素数が (q+1)*x
                // < のときあまりはすべてペアになってるので適当にどこかを長くすればいい、気にしなくていい
                let ok1 = (q + 1) * x <= n;
                // q 個の部分列に分割して >=x+1 を達成できる？
                let ok2 = q * (x + 1) <= n;
                ok1 || ok2
            } else {
                // q 個の部分列に分割
                q * x <= n
            };

            *(if ok { &mut xl } else { &mut xr }) = x;
        }

        xl
    };

    println!("{}", x);
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
