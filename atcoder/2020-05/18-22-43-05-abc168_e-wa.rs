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

pub fn factorize(mut x: i64) -> BTreeMap<i64, i64> {
    let mut ms = BTreeMap::new();
    let r = (x as f64).sqrt() as i64 + 1;

    for p in 2..r as i64 {
        let mut m = 0;

        while x >= p && x % p == 0 {
            x /= p;
            m += 1;
        }

        if m > 0 {
            ms.insert(p, m);
        }
    }

    if x > 1 {
        ms.insert(x, 1);
    }

    ms
}

fn sub(left: &mut BTreeMap<i64, i64>, right: &BTreeMap<i64, i64>) {
    for (k, v) in right {
        *left.entry(*k).or_insert(0) -= *v;

        if left[k] == 0 {
            left.remove(k);
        }
    }
}

const P: i64 = 1_000_000_007;

type V2 = Vec<(i64, i64)>;

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
    let mut vec = vec![];

    for _ in 0..N {
        let A = scan.word::<i64>();
        let B = scan.word::<i64>();

        match (A, B) {
            (0, 0) => zz += 1,
            (0, _) => az += 1,
            (_, 0) => bz += 1,
            _ => {}
        }

        // A/B の符号
        let signum = if A.signum() == B.signum() { 0 } else { 1 };

        // a = factorize(A/B)
        let a = {
            let mut a = factorize(A.abs());
            let b = factorize(B.abs());
            sub(&mut a, &b);
            a
        };

        vec.clear();
        vec.extend(a.iter().map(|(&k, &v)| (k, v)));

        freq.entry(vec.clone()).or_insert([0, 0])[signum] += 1;
    }

    // orth[] = (x, y) : x 匹のサバと y 匹のサバは互いに相性が悪い。
    let mut orth = vec![(az, bz)];
    let mut tan = vec![];

    loop {
        match freq.keys().next() {
            None => break,
            Some(v) => {
                vec.clear();
                vec.extend(v.iter());

                tan.clear();
                tan.extend(v.iter().map(|&(k, v)| (k, -v)));
            }
        }

        for signum in 0..2 {
            let x = freq
                .get_mut(&vec)
                .map(|xs| std::mem::take(&mut xs[signum]))
                .unwrap_or(0);

            let y = freq
                .get_mut(&tan)
                .map(|ys| std::mem::take(&mut ys[1 - signum]))
                .unwrap_or(0);

            if x == 0 && y == 0 {
                continue;
            }

            orth.push((x, y));
        }

        let xs = freq.remove(&vec);
        debug_assert_eq!(xs, Some([0, 0]));
    }

    // A = B = 0 のサバは他のすべてのサバと相性が悪いので、
    // このようなサバ x を含む選び方は {x} しかない。
    total += zz;

    let mut ab = 1_i64;

    for (x, y) in orth {
        // x 匹のサバからなるグループと y 匹のサバからなるグループは互いに仲が悪いので、
        // 「サバを1匹も選ばない」「x 匹のグループから 1..=x 匹を選ぶ」「y 匹のグループから 1..=y 匹を選ぶ」
        // の3通りのケースを考える。
        // 他のサバに関しては好きに選んでいいので、場合の数は掛け合わせていく。
        ab *= (pow(2, x as i64) + pow(2, y as i64) - 1) % P;
        ab %= P;
    }

    total += ab;
    total %= P;

    total += P - 1;
    total %= P;

    println!("{}", total);
}
