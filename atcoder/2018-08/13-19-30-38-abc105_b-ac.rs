#![allow(non_snake_case)]

use std::io::stdin;

pub fn main() {
    let stdin = stdin();
    let mut buf = String::new();
    stdin.read_line(&mut buf).unwrap();

    let N: i32 = buf.trim_right().parse().unwrap();

    let mut ok = false;
    'a: for x in 0..100 {
        for y in 0..100 {
            if N == 7 * x + 4 * y {
                ok = true;
                break 'a;
            }
        }
    }

    if ok {
        println!("Yes")
    } else {
        println!("No")
    }
}
