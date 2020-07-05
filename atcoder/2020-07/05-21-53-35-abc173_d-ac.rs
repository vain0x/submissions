//! Framework <https://github.com/vain0x/procon>

#![allow(non_snake_case)]
#![allow(unused_imports)]

use std::collections::*;

pub struct Scan(Box<dyn Iterator<Item = &'static str>>); // '

impl Scan {
    fn new() -> Self {
        let mut buf = String::new();
        let read_line = move || {
            std::io::stdin().read_line(&mut buf).unwrap();
            Box::leak(buf.split_off(0).into_boxed_str()).split_whitespace()
        };
        Scan(Box::new(std::iter::repeat_with(read_line).flatten()))
    }

    pub fn word<T: std::str::FromStr>(&mut self) -> T {
        self.0.next().unwrap().parse().ok().unwrap()
    }

    pub fn list<T: std::str::FromStr>(&mut self, len: usize) -> Vec<T> {
        std::iter::repeat_with(|| self.word()).take(len).collect()
    }
}

#[allow(unused)]
pub fn next_perm<T: Ord>(xs: &mut [T]) -> bool {
    // `xs[i + 1..]` : desc but
    // `xs[i..]` : not desc.
    let i = match (0..xs.len())
        .rev()
        .filter(|&i| i + 1 < xs.len() && xs[i] < xs[i + 1])
        .next()
    {
        None => return false,
        Some(i) => i,
    };

    // `xs[k]` : The next greater elem in `xs[i..]`.
    let k = (i + 1..xs.len())
        .rev()
        .filter(|&k| xs[i] < xs[k])
        .next()
        .unwrap();

    // E.g. 2431 -> 3421 -> 3124 (where i = 0, k = 2).
    xs.swap(i, k);
    xs[i + 1..].reverse();

    true
}

#[allow(unused)]
pub fn brute_force(N: usize) {
    let A = (0..N).map(|i| i + 1).collect::<Vec<_>>();
    let mut perm = (0..N).collect::<Vec<_>>();
    let mut max_total = 0;

    loop {
        let mut rev = vec![0; N];
        for i in 0..N {
            rev[perm[i]] = i;
        }

        let mut left = perm.clone();
        let mut right = perm.clone();

        for i in 0..N {
            left[i] = if perm[i] == 0 {
                rev[N - 1]
            } else {
                rev[perm[i] - 1]
            };

            right[i] = if perm[i] == N - 1 {
                rev[0]
            } else {
                rev[perm[i] + 1]
            };
        }

        // println!("perm={:?}", perm);
        // println!("left={:?}", left);
        // println!("right={:?}", right);

        let mut ord = (0..N).collect::<Vec<_>>();
        loop {
            let mut total = 0;

            for i in 0..N {
                if ord[i] == 0 {
                    continue;
                }

                let l = std::iter::successors(Some(left[i]), |&i| Some(left[i]))
                    .find(|&j| ord[j] < ord[i])
                    .unwrap();
                let r = std::iter::successors(Some(right[i]), |&i| Some(right[i]))
                    .find(|&j| ord[j] < ord[i])
                    .unwrap();

                total += A[l].min(A[r]);
            }

            println!("perm={:?}", perm);
            println!("ord={:?}", ord);
            println!("total={}\n", total);
            max_total = max_total.max(total);

            if !next_perm(&mut ord) {
                break;
            }
        }

        if !next_perm(&mut perm) {
            break;
        }
    }

    println!("max={}", max_total);
}

fn main() {
    // brute_force(4);

    let mut scan = Scan::new();

    let N = scan.word::<usize>();
    let mut A = scan.list::<usize>(N);
    A.sort();

    let mut total = A[N - 1];
    for i in 2..N {
        total += A[N - (i / 2) - 1];
    }

    println!("{}", total);
}
