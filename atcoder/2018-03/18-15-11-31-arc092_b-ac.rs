#![allow(unused_imports)]
#![allow(non_snake_case)]

use std::*;
use std::collections::*;
use std::ops::*;
use procon::*;

const BITSIZE: usize = 30;

pub fn intercalate<T: ToString>(xs: &[T], separator: &str) -> String {
    xs.into_iter()
        .map(|x| x.to_string())
        .collect::<Vec<_>>()
        .join(separator)
}

fn shakutori(A: &[u64], B: &Vec<u64>, l: &mut usize, low: u64, high: u64) -> usize {
    let N = A.len();
    let mut parity = 0;
    let mut l = *l;
    let mut r = l;

    for &a in A {
        while l < N && a + B[l] < low {
            l += 1;
        }
        assert!(l == N || a + B[l] >= low);

        // r = r.max(l);
        r = if l < r { r } else { l };
        while r < N && a + B[r] < high {
            r += 1;
        }
        assert!(r == N || a + B[r] >= high);

        /*
        eprintln!("a={} l={} r={}", a, l, r);
        */
        parity ^= (r - l) & 1;
    }

    parity
}

pub fn main() {
    let _N = read_line().parse::<usize>().unwrap();
    let A: Vec<u64> = read_words();
    let B: Vec<u64> = read_words();
    let mut x = 0;

    for k in 0..BITSIZE {
        let T = 1 << k;
        let mut A = A.iter().cloned().map(|a| a % (2 * T)).collect::<Vec<_>>();
        // A.sort_by_key(|&x| std::cmp::Reverse(x));
        A.sort();
        A.reverse();

        let mut B = B.iter().cloned().map(|b| b % (2 * T)).collect::<Vec<_>>();
        B.sort();

        let mut l1 = 0;
        let mut l2 = 0;

        // a + b が T 以上 2T 未満になるような (a, b) の個数の偶奇
        let p1 = shakutori(A.as_slice(), &B, &mut l1, T, 2 * T);

        let p2 = shakutori(A.as_slice(), &B, &mut l2, 3 * T, 4 * T);

        /*
        eprintln!(
            "k={} p={} A={} B={}",
            k,
            p1 ^ p2,
            intercalate(&A, ", "),
            intercalate(&B, ", ")
        );
        */

        x |= (p1 ^ p2) << k;
    }

    println!("{}", x);
    return;
}

pub mod procon {
    use std;
    use std::collections::*;
    use std::io;
    use std::mem;
    use std::str::FromStr;

    pub fn read_line() -> String {
        let mut buf = String::new();
        io::stdin().read_line(&mut buf).unwrap();
        buf.trim_right().to_owned()
    }

    pub fn read_words<T>() -> Vec<T>
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

    pub fn read_vec(len: usize) -> Vec<String> {
        let mut vec = Vec::new();
        while vec.len() < len {
            let line = read_line();
            for word in line.split_whitespace() {
                vec.push(word.to_owned());
            }
        }
        assert!(vec.len() == len);
        vec
    }
}
