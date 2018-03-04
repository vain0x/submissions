#![allow(unused_imports)]
#![allow(non_snake_case)]

use std::*;
use std::collections::*;
use std::ops::*;
use procon::*;

pub fn main() -> () {
    let N = read_line().parse::<usize>().unwrap();
    let names = read_vec(N);

    let counts = {
        let mut map = "MARCH"
            .chars()
            .map(|c| (c, 0))
            .collect::<HashMap<char, i64>>();

        for name in names {
            let c = name.chars().next().unwrap();
            if let Some(value) = map.get_mut(&c) {
                *value += 1;
            }
        }

        map.values().cloned().collect::<Vec<i64>>()
    };

    let combi = vec![
        (0, 1, 2),
        (0, 1, 3),
        (0, 1, 4),
        (0, 2, 3),
        (0, 2, 4),
        (0, 3, 4),
        (1, 2, 3),
        (1, 2, 4),
        (1, 3, 4),
        (2, 3, 4),
    ];
    let mut sum = 0;
    for (i, j, k) in combi {
        sum += counts[i] * counts[j] * counts[k];
    }

    println!("{}", sum);
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

    pub fn make_vec<T, F>(len: usize, f: F) -> Vec<T>
    where
        F: Fn(usize) -> T,
    {
        return (0..len).into_iter().map(f).collect();
    }
}
