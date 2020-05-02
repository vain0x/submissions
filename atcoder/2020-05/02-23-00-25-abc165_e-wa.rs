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

fn validate(N: usize, M: usize, assignment: &[(usize, usize)]) -> bool {
    let mut freq = HashMap::new();

    for x in 0..N {
        for i in 0..M {
            let (u, v) = assignment[i];

            let u = (u + x - 1) % N + 1;
            let v = (v + x - 1) % N + 1;
            *freq.entry((u, v)).or_insert(0) += 1_usize;
            *freq.entry((v, u)).or_insert(0) += 1_usize;

            if freq[&(u, v)] > 1 || freq[&(v, u)] > 1 {
                #[cfg(debug_assertions)]
                eprintln!(
                    "\x1B[33m{}\x1B[0m = {:?}",
                    "(N, M, &assignment, x, i, u, v)",
                    (N, M, &assignment, x, i, u, v)
                );
                return false;
            }
        }
    }

    let mut freq = HashMap::new();
    for (u, v) in assignment {
        *freq.entry(u).or_insert(0) += 1_usize;
        *freq.entry(v).or_insert(0) += 1_usize;
    }

    for u in 1..=N {
        if freq.get(&u).cloned().unwrap_or(0) > 1 {
            #[cfg(debug_assertions)]
            eprintln!("\x1B[33m{}\x1B[0m = {:?}", "u", u);
            return false;
        }
    }

    true
}

fn check() {
    let mut assignment = vec![];
    for N in 3..30 {
        let mut M = 1;
        while M * 2 + 1 <= N {
            assignment.clear();
            for x in 0..M {
                assignment.push((x + 1, x + M - 1 + (x + 1) + 1));
            }

            if !validate(N, M, &assignment) {
                break;
            }

            M += 1;
        }
    }
}

fn main() {
    let mut scan = Scan::new();

    let N = scan.word::<usize>();
    let M = scan.word::<usize>();

    let mut assignment = vec![];
    let mut done = vec![false; N + 1];
    let mut l = 0;
    let mut r = N;

    for x in (1..=M).rev() {
        let (mut i, step) = if x % 2 == 0 { (l, 1) } else { (r - x - 1, -1) };

        while l <= i && i + x < r {
            if !done[i] && !done[i + x] {
                assignment.push((i + 1, i + x + 1));

                done[i] = true;
                if i == l {
                    l += 1;
                }

                done[i + x] = true;
                if i + x == r - 1 {
                    r -= 1;
                }
                break;
            }

            i = (i as isize + step) as usize;
        }
    }

    debug_assert!(validate(N, M, &assignment));

    for &(u, v) in &assignment {
        println!("{} {}", u + 1, v + 1);
    }
}
