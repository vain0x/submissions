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
    let words = rw::<String>();
    let N: usize = words[0].parse().unwrap();
    let D: i64 = words[1].parse().unwrap();
    let A: Vec<i64> = rw();

    // disz[i]: X_0 から X_i までの距離
    let mut disz = vec![0_i64; N];
    for i in 1..N {
        disz[i] = disz[i - 1] + A[i] - A[i - 1];
    }

    // disz(l, r): X_l から X_r までの距離
    let dis = move |l: usize, r: usize| disz[r] - disz[l];

    // p[i] = i < j かつ i-jの距離がD以下であるような (i, j) の数
    let mut p = vec![0_i64; N];

    // q[i] = sum(p[i..])
    let mut q = vec![0_i64; N + 1];

    {
        let mut l = 0;
        let mut r = 0; // inclusive
        let mut dist = 0;
        while l < N {
            while r + 1 < N && dist + A[r + 1] - A[r] <= D {
                dist += A[r + 1] - A[r];
                r += 1;
            }

            // l <= r, l < N, r < N, l-r間の距離はD以下
            p[l] += r as i64 - l as i64;

            if l + 1 < N {
                dist -= A[l + 1] - A[l];
            }
            l += 1;
        }
    }

    {
        for i in (0..N).rev() {
            q[i] = q[i + 1] + p[i];
        }
    }

    {
        let mut total = 0_i64;
        let mut il = 0;
        let mut kr = 0;

        for j in 1..N - 1 {
            // i, k ともに j との距離が D 以下でなければならない
            // ただし i-k の距離がD以下のペアを数えてしまうので、後で引く

            il = min(il, j);
            while dis(il, j) > D {
                il += 1;
            }
            // il..j は i の候補

            while kr < N && dis(j, kr) <= D {
                kr += 1;
            }
            // kr >= N || dis(j, kr) > D
            // j+1..kr は k の候補

            total += (j - il) as i64 * ((kr - j) as i64 - 1);
        }

        let mut kr = 0;
        for i in 0..N {
            // i-kの距離がD以下のペアの数を引いていく

            while kr + 1 < N && dis(i, kr + 1) <= D {
                kr += 1;
            }
            // i-kr は距離 D 以下
            // i+1 .. kr+1 から2つ選ぶ
            let n = (i + 1..kr + 1).len() as i64;
            total -= n * (n - 1) / 2;
        }

        println!("{}", total);
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
