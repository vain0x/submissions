use std::io::*;

#[allow(dead_code)]
fn rl() -> String {
    let mut buf = String::new();
    stdin().read_line(&mut buf).unwrap();
    buf.trim_right().to_owned()
}

pub fn main() {
    println!(
        "{}",
        if rl().starts_with("MUJIN") {
            "Yes"
        } else {
            "No"
        }
    );
}
