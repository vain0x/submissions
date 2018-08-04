use std::io::*;

#[allow(dead_code)]
fn rl() -> String {
    let mut buf = String::new();
    stdin().read_line(&mut buf).unwrap();
    buf.trim_right().to_owned()
}

pub fn main() {
    let A = rl().parse::<usize>().unwrap();
    let S = rl();

    let mut n = A;

    let mut ok = n > 0;
    for c in S.chars() {
        match c {
            '+' => n += 1,
            '-' => {
                if n == 1 {
                    ok = false;
                    break;
                }
                n -= 1;
            }
            _ => unreachable!(),
        }
    }

    println!("{}", if !ok { "Yes" } else { "No" });
}
