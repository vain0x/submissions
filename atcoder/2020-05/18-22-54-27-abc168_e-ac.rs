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

pub fn gcd(x: i64, y: i64) -> i64 {
    if y == 0 {
        x.abs()
    } else {
        gcd(y, x % y)
    }
}

pub fn pow(x: i64, n: i64) -> i64 {
    let (mut x, mut y, mut n) = (x % P, 1_i64, n);
    while n > 0 {
        if n % 2 != 0 {
            y = (y * x) % P;
            n -= 1;
        }

        x = (x * x) % P;
        n /= 2;
    }
    y
}

const P: i64 = 1_000_000_007;

type V2 = (i64, i64);

fn main() {
    let mut scan = Scan::new();

    let N = scan.word::<usize>();
    let mut total = 0_i64;

    // A = 0, B != 0
    let mut az = 0;
    // A != 0, B = 0
    let mut bz = 0;
    // A = B = 0
    let mut zz = 0;

    let mut freq = HashMap::<V2, [usize; 2]>::new();

    for _ in 0..N {
        let A = scan.word::<i64>();
        let B = scan.word::<i64>();

        match (A, B) {
            (0, 0) => {
                zz += 1;
                continue;
            }
            (0, _) => {
                az += 1;
                continue;
            }
            (_, 0) => {
                bz += 1;
                continue;
            }
            _ => {}
        }

        let (r, s) = {
            let mut a = A.abs();
            let mut b = B.abs();
            let g = gcd(a, b);
            a /= g;
            b /= g;
            if A.signum() == B.signum() {
                ((a, b), 0)
            } else {
                ((b, a), 1)
            }
        };

        freq.entry(r).or_insert([0, 0])[s] += 1;
    }

    // A = B = 0 のサバは他のすべてのサバと相性が悪いので、
    // このようなサバ x を含む選び方は {x} しかない。
    total += zz;

    let mut ab = 1_i64;

    for (_, xs) in freq {
        let (x, y) = (xs[0], xs[1]);

        // x 匹のサバからなるグループと y 匹のサバからなるグループは互いに仲が悪いので、
        // 「サバを1匹も選ばない」「x 匹のグループから 1..=x 匹を選ぶ」「y 匹のグループから 1..=y 匹を選ぶ」
        // の3通りのケースを考える。
        // 他のサバに関しては好きに選んでいいので、場合の数は掛け合わせていく。
        ab *= (pow(2, x as i64) + pow(2, y as i64) - 1) % P;
        ab %= P;
    }

    ab *= (pow(2, az) + pow(2, bz) - 1) % P;
    ab %= P;

    // サバを1匹も選ばないケースは除く。
    ab += P - 1;

    total += ab;
    total %= P;

    println!("{}", total);
}
