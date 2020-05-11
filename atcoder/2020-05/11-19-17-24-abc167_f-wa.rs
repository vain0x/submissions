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

#[derive(PartialEq, Clone, Debug)]
struct Interval<T> {
    l: T,
    r: T,
}

impl<T: Ord> Interval<T> {
    fn new(l: T, r: T) -> Interval<T> {
        Interval { l, r }
    }

    fn disjoint(&self, other: &Self) -> bool {
        self.r <= other.l || other.r <= self.l
    }

    fn covers(&self, other: &Self) -> bool {
        self.l <= other.l && other.r <= self.r
    }
}

#[derive(Debug)]
pub struct SegTree<T, F> {
    len: usize,

    /// Number of leaf nodes.
    width: usize,

    /// len = `2w-1` for `w-1` inners and `w` leaves,
    /// where `w` is the smallest `2^p` (`>= len`).
    node: Vec<T>,

    mempty: T,
    mappend: F,
}

impl<T, F> SegTree<T, F>
where
    T: Clone,
    F: Fn(T, T) -> T,
{
    pub fn new(items: Vec<T>, mempty: T, mappend: F) -> SegTree<T, F> {
        let len = items.len();

        let mut w = 1;
        while w < len {
            w *= 2;
        }
        debug_assert!(w >= len);

        let mut node = vec![mempty.clone(); 2 * w - 1];

        for (ei, item) in items.into_iter().enumerate() {
            node[w - 1 + ei] = item;
        }

        for ni in (0..w - 1).rev() {
            node[ni] = mappend(node[2 * ni + 1].clone(), node[2 * ni + 2].clone());
        }

        SegTree {
            len: len,
            width: w,
            node: node,
            mempty: mempty,
            mappend: mappend,
        }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn set(&mut self, ei: usize, value: T) {
        let mut ni = self.width - 1 + ei;
        self.node[ni] = value;

        while ni > 0 {
            ni = (ni - 1) / 2;
            self.node[ni] =
                (self.mappend)(self.node[2 * ni + 1].clone(), self.node[2 * ni + 2].clone());
        }
    }

    pub fn sum(&self, ql: usize, qr: usize) -> T {
        let q = Interval::new(ql, qr);
        if q.disjoint(&Interval::new(0, self.len())) {
            self.mempty.clone()
        } else {
            self.sum_core(0, Interval::new(0, self.width), &q)
        }
    }

    fn sum_core(&self, ni: usize, e: Interval<usize>, q: &Interval<usize>) -> T {
        if e.disjoint(&q) {
            self.mempty.clone()
        } else if q.covers(&e) {
            self.node[ni].clone()
        } else {
            let m = (e.l + e.r) / 2;
            let vl = self.sum_core(2 * ni + 1, Interval::new(e.l, m), q);
            let vr = self.sum_core(2 * ni + 2, Interval::new(m, e.r), q);
            (self.mappend)(vl.clone(), vr.clone())
        }
    }
}

fn calc(s: &str) -> (usize, i64) {
    // バランス。左カッコと右カッコの個数の差を表す。
    let mut balance = 0;

    // 最小バランス。カッコ列をバランスさせるために s の左側に何個の '(' を足す必要があるかを表す。
    let mut least = 0;

    for c in s.chars() {
        match c {
            '(' => balance += 1,
            ')' => balance -= 1,
            _ => unreachable!(),
        }
        least = least.min(balance);
    }

    ((-least) as usize, balance)
}

fn main() {
    let mut scan = Scan::new();

    let N = scan.word::<usize>();
    let S = scan.list::<String>(N);

    // 総文字数 (バランスの絶対値の上限)
    let M = S.iter().map(|s| s.len()).sum::<usize>();

    // カッコ列を (least, balance) に関して整理する。
    // クエリを高速に処理するために、マップとセグメントツリーに分けて持つ。
    let mut map = BTreeMap::new();
    for s in &S {
        let (least, balance) = calc(s);
        assert!(least <= M);

        map.entry(least).or_insert(vec![]).push(balance);
    }

    for v in map.values_mut() {
        v.sort();
    }

    let none = (std::i64::MIN, N);
    let mut seg = SegTree::new(vec![none; M + 1], none, std::cmp::max);
    for (&least, balances) in &map {
        let max_balance = *balances.last().unwrap();
        seg.set(least, (max_balance, least));
    }

    // いくつかの文字列を連結していく。
    // 現在の文字列のバランス値
    let mut balance = 0_i64;
    let mut ok = true;

    for _ in 0..N {
        // いま x = balance 個だけ '(' が余っている。
        // least が x より大きい文字列を次に連結すると、')' が対応しなくなり、破綻する。
        // そのため、候補は least が x 以下である文字列に絞られる。
        // そのような文字列のうち、balance が最大であるものを連結するのがベスト。(たぶん)
        let (max_balance, least) = seg.sum(0, (balance + 1).max(0) as usize);

        let balances = match map.get_mut(&least) {
            None => {
                // どの文字列を連結しても ')' が余る。
                ok = false;
                break;
            }
            Some(v) => v,
        };

        // 選ばれた文字列を削除する。(マップとセグメントツリーを更新。)
        let b = balances.pop().unwrap();
        debug_assert_eq!(b, max_balance);

        let second = match balances.last().cloned() {
            None => {
                map.remove(&least);
                none
            }
            Some(balance) => (balance, least),
        };
        seg.set(least, second);

        // 選ばれた文字列を連結した後の状態を計算する。
        balance += max_balance;
        debug_assert!(balance >= 0);
    }

    ok = ok && balance == 0;

    println!("{}", if ok { "Yes" } else { "No" });
}
