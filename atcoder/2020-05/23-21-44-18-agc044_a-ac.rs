//! Framework <https://github.com/vain0x/procon>

#![allow(non_snake_case)]
#![allow(unused_imports)]

use std::{cmp::Reverse, collections::*};

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

fn main() {
    let mut scan = Scan::new();

    let T = scan.word::<usize>();

    for _ in 0..T {
        let N = scan.word::<i64>();
        let A = scan.word::<i64>();
        let B = scan.word::<i64>();
        let C = scan.word::<i64>();
        let D = scan.word::<i64>();

        let mut q = BinaryHeap::new();
        let mut dist = HashMap::new();

        q.push((Reverse(0), N));
        dist.insert(N, 0);

        let mut update = |cost, v, q: &mut BinaryHeap<_>| {
            if let Some(&old_cost) = dist.get(&v) {
                if old_cost <= cost {
                    return;
                }
            }

            dist.insert(v, cost);
            q.push((Reverse(cost), v));
        };

        while let Some((Reverse(cost), u)) = q.pop() {
            if u >= 1 {
                if let Some(cost) = u.checked_mul(D).and_then(|n| n.checked_add(cost)) {
                    update(cost, 0, &mut q);
                }
            }

            if u >= 2 {
                update(cost + (u % 2) * D + A, u / 2, &mut q);
                update(cost + (2 - u % 2) * D + A, u / 2 + 1, &mut q);
            }

            if u >= 3 {
                update(cost + (u % 3) * D + B, u / 3, &mut q);
                update(cost + (3 - u % 3) * D + B, u / 3 + 1, &mut q);
            }

            if u >= 5 {
                update(cost + (u % 5) * D + C, u / 5, &mut q);
                update(cost + (5 - u % 5) * D + C, u / 5 + 1, &mut q);
            }
        }

        println!("{}", dist[&0]);
    }
}
