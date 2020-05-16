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

// 解説AC

use std::cmp::Reverse;

/// カッコからなる文字列を解析した結果
struct Brackets {
    /// 文字列内に対応する '(' がない ')' の個数。(editorials の A(i))
    down: i64,
    /// 文字列内に対応する ')' がない '(' の個数。(B(i))
    up: i64,
}

fn calc(s: &str) -> Brackets {
    let mut left = 0;
    let mut right = 0;
    let mut down = 0;

    for c in s.chars() {
        match c {
            '(' => left += 1,
            ')' => right += 1,
            _ => unreachable!(),
        };

        down = down.max(right - left);
    }

    let up = left - (right - down);
    Brackets { down, up }
}

fn main() {
    let mut scan = Scan::new();

    let N = scan.word::<usize>();
    let S = scan.list::<String>(N);

    // '(' が多い、なるべく先に連結したいカッコ列
    let mut good = vec![];
    // ')' が多い、なるべく後まで連結したくないカッコ列
    let mut bad = vec![];

    for s in S {
        let b = calc(&s);
        if b.up > b.down {
            good.push(b);
        } else {
            bad.push(b);
        }
    }

    good.sort_by_key(|b| b.down);
    bad.sort_by_key(|b| Reverse(b.up));

    let mut balance = 0;
    let mut ok = true;
    for &Brackets { down, up } in good.iter().chain(&bad) {
        if balance < 0 || balance < down {
            ok = false;
            break;
        }

        balance -= down;
        balance += up;
    }
    ok = ok && balance == 0;

    println!("{}", if ok { "Yes" } else { "No" });
}
