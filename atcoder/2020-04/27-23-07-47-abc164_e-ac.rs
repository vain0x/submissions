//! Framework <https://github.com/vain0x/procon>
//! 解説AC + ランダムテスト

#![allow(non_snake_case)]
#![allow(unused_imports)]

use std::cmp::Reverse;
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

const MAX_CASH: usize = 50 * (50 - 1);

fn solve(
    initial_cash: usize,
    graph: &[Vec<(usize, usize, usize)>],
    exchange_cash: &[usize],
    exchange_time: &[usize],
) -> Vec<usize> {
    let N = graph.len();

    let initial_cash = initial_cash.min(MAX_CASH);

    // queue[] = (rev 移動時間, 所持銀貨, 都市)
    let mut queue = BinaryHeap::new();

    // dist[u][cash] = t
    // 都市 0 から都市 u に至る経路で、
    // u に到着したときの所持銀貨が cash 枚になるとき、
    // 移動時間の最小値が t
    let mut dist = vec![vec![std::usize::MAX; MAX_CASH + 1]; N];

    // Dijkstra する。
    dist[0][initial_cash] = 0;
    queue.push((Reverse(0), initial_cash, 0));

    while let Some((Reverse(time), cash, u)) = queue.pop() {
        // 両替
        {
            let next_cash = (cash + exchange_cash[u]).min(MAX_CASH);
            let next_time = time + exchange_time[u];

            if dist[u][next_cash] > next_time {
                dist[u][next_cash] = next_time;
                queue.push((Reverse(next_time), next_cash, u));
            }
        }

        // 都市間の移動
        for &(v, A, B) in &graph[u] {
            if cash < A {
                continue;
            }

            let next_cash = cash - A;
            let next_time = time + B;

            if dist[v][next_cash] > next_time {
                dist[v][next_cash] = next_time;
                queue.push((Reverse(next_time), next_cash, v));
            }
        }
    }

    (0..N)
        .map(|u| dist[u].iter().min().cloned().unwrap())
        .collect()
}

fn main() {
    let mut scan = Scan::new();

    let N = scan.word::<usize>();
    let M = scan.word::<usize>();
    let S = scan.word::<usize>();

    let mut graph = vec![vec![]; N];
    for _ in 0..M {
        let U = scan.word::<usize>() - 1;
        let V = scan.word::<usize>() - 1;
        let A = scan.word::<usize>();
        let B = scan.word::<usize>();
        graph[U].push((V, A, B));
        graph[V].push((U, A, B));
    }

    let mut C = vec![];
    let mut D = vec![];
    for _ in 0..N {
        C.push(scan.word::<usize>());
        D.push(scan.word::<usize>());
    }

    let dist = solve(S, &mut graph, &C, &D);

    for t in 1..N {
        println!("{}", dist[t]);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn solve_str(input: &str, output: &str) {
        let mut scan = Scan(Box::new(
            Box::leak(input.to_string().into_boxed_str()).split_whitespace(),
        ));

        let N = scan.word::<usize>();
        let M = scan.word::<usize>();
        let S = scan.word::<usize>();

        let mut graph = vec![vec![]; N];
        for _ in 0..M {
            let U = scan.word::<usize>() - 1;
            let V = scan.word::<usize>() - 1;
            let A = scan.word::<usize>();
            let B = scan.word::<usize>();
            graph[U].push((V, A, B));
            graph[V].push((U, A, B));
        }

        let mut C = vec![];
        let mut D = vec![];
        for _ in 0..N {
            C.push(scan.word::<usize>());
            D.push(scan.word::<usize>());
        }

        let dist = solve(S, &mut graph, &C, &D);

        let expected = output
            .split_whitespace()
            .map(|x| x.parse().unwrap())
            .collect::<Vec<_>>();
        assert_eq!(&dist[1..], expected.as_slice());
    }

    #[test]
    fn case1() {
        solve_str(
            r##"
                3 2 1
                1 2 1 2
                1 3 2 4
                1 11
                1 2
                2 5
            "##,
            r##"
                2
                14
            "##,
        );
    }

    #[test]
    fn case2() {
        solve_str(
            r#"4 4 1
        1 2 1 5
        1 3 4 4
        2 4 2 2
        3 4 1 1
        3 1
        3 1
        5 2
        6 4
        "#,
            r#"5
        5
        7
        "#,
        );
    }

    #[test]
    fn case3() {
        solve_str(
            r#"6 5 1
        1 2 1 1
        1 3 2 1
        2 4 5 1
        3 5 11 1
        1 6 50 1
        1 10000
        1 3000
        1 700
        1 100
        1 1
        100 1
        "#,
            r#"1
        9003
        14606
        16510
        16576
        "#,
        );
    }

    #[test]
    fn case4() {
        solve_str(
            r#"4 6 1000000000
        1 2 50 1
        1 3 50 5
        1 4 50 7
        2 3 50 2
        2 4 50 4
        3 4 50 3
        10 2
        4 4
        5 5
        7 7
        "#,
            r#"1
        3
        5
        "#,
        );
    }

    #[test]
    fn case5() {
        solve_str(
            r#"2 1 0
        1 2 1 1
        1 1000000000
        1 1
        "#,
            r#"1000000001
        "#,
        );
    }

    fn brute_force(
        initial_cash: usize,
        graph: &[Vec<(usize, usize, usize)>],
        exchange_cash: &[usize],
        exchange_time: &[usize],
    ) -> Vec<usize> {
        let N = graph.len();

        let mut dist = vec![std::usize::MAX; N];
        let mut dist_count = 0;

        dist[0] = 0;
        dist_count += 1;

        // BFS
        let mut queue = BinaryHeap::new();
        queue.push((Reverse(0), initial_cash, 0));

        while let Some((Reverse(time), cash, u)) = queue.pop() {
            if dist[u] == std::usize::MAX {
                dist[u] = time;
                dist_count += 1;

                if dist_count >= N {
                    break;
                }
            }

            queue.push((Reverse(time + exchange_time[u]), cash + exchange_cash[u], u));

            for i in 0..graph[u].len() {
                let (v, A, B) = graph[u][i];
                if cash < A {
                    continue;
                }

                queue.push((Reverse(time + B), cash - A, v));
            }
        }

        dist
    }

    #[test]
    fn verify() {
        use rand::seq::SliceRandom;
        use rand::{Rng, SeedableRng};

        let mut rng = rand::rngs::SmallRng::from_entropy();

        for (N, round) in (2..7).flat_map(|N| (0..21 / N).map(move |k| (N, k))) {
            eprintln!("N = {}, round = {}", N, round);

            let initial_cash = rng.gen_range(0, 100);

            let mut edges = HashSet::new();

            let mut perm = (0..N).collect::<Vec<_>>();
            perm.shuffle(&mut rng);

            for pair in perm.windows(2) {
                let (u, v) = match pair {
                    &[u, v] => (u, v),
                    _ => unreachable!(),
                };

                edges.insert((u, v));
            }

            for u in 0..N {
                for v in u + 1..N {
                    if !edges.contains(&(u, v)) && rng.gen() {
                        edges.insert((u, v));
                    }
                }
            }

            let mut graph = vec![vec![]; N];
            for &(u, v) in &edges {
                let A: usize = rng.gen_range(1, 50 + 1);
                let B: usize = rng.gen_range(1, 100);
                graph[u].push((v, A, B));
                graph[v].push((u, A, B));
            }

            let C = (0..N)
                .map(|_| rng.gen_range(1, 100))
                .collect::<Vec<usize>>();
            let D = (0..N)
                .map(|_| rng.gen_range(1, 100))
                .collect::<Vec<usize>>();

            let expected = brute_force(initial_cash, &graph, &C, &D);
            let actual = solve(initial_cash, &graph, &C, &D);
            assert_eq!(actual, expected, "{:?}", (initial_cash, &graph, &C, &D));
        }
    }
}
