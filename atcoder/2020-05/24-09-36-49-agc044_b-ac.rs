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

fn main() {
    let mut scan = Scan::new();

    let N = scan.word::<usize>();
    let M = N * N;

    let P = scan.list::<usize>(M);

    let mut index = vec![vec![M; N]; N];
    let mut u = 0;
    for y in 0..N {
        for x in 0..N {
            index[y][x] = u;
            u += 1;
        }
    }

    let mut g = vec![vec![]; M];
    for y in 0..N {
        for x in 0..N {
            let u = index[y][x];

            for &(dy, dx) in &[(1, 0), (0, -1), (-1, 0), (0, 1)] {
                let (y2, x2) = (y as isize + dy, x as isize + dx);
                if y2 < 0 || x2 < 0 || y2 >= N as isize || x2 >= N as isize {
                    continue;
                }
                let (y2, x2) = (y2 as usize, x2 as usize);

                let v = index[y2][x2];
                g[u].push(v);
                g[v].push(u);
            }
        }
    }

    let mut hated = vec![M; M];
    for y in 0..N {
        for x in 0..N {
            let u = index[y][x];
            hated[u] = y.min(x).min(N - 1 - y).min(N - 1 - x);
        }
    }

    let mut q = VecDeque::new();
    let mut occupied = vec![1; M];
    let mut total = 0;

    for p in P {
        let u = p - 1;

        total += hated[u];
        occupied[u] = 0;

        q.clear();
        q.push_back((u, hated[u]));

        while let Some((u, d)) = q.pop_front() {
            let d = d + occupied[u];

            for i in 0..g[u].len() {
                let v = g[u][i];

                if hated[v] > d {
                    hated[v] = d;
                    q.push_back((v, d));
                }
            }
        }
    }

    println!("{}", total);
}
