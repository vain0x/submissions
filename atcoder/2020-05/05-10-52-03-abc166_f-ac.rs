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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum Var {
    A,
    B,
    C,
}

impl Var {
    fn as_str(self) -> &'static str {
        match self {
            Var::A => "A",
            Var::B => "B",
            Var::C => "C",
        }
    }
}

#[derive(Clone, Copy)]
enum Command {
    AB,
    AC,
    BC,
}

impl Command {
    fn parse(s: &str) -> Command {
        match s {
            "AB" => Command::AB,
            "AC" => Command::AC,
            "BC" => Command::BC,
            _ => panic!(),
        }
    }

    fn vars(self) -> (Var, Var, Var) {
        match self {
            Command::AB => (Var::A, Var::B, Var::C),
            Command::AC => (Var::A, Var::C, Var::B),
            Command::BC => (Var::B, Var::C, Var::A),
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
struct State {
    A: usize,
    B: usize,
    C: usize,
}

impl State {
    fn get(self, v: Var) -> usize {
        match v {
            Var::A => self.A,
            Var::B => self.B,
            Var::C => self.C,
        }
    }

    fn get_mut(&mut self, v: Var) -> &mut usize {
        match v {
            Var::A => &mut self.A,
            Var::B => &mut self.B,
            Var::C => &mut self.C,
        }
    }
}

fn solve(mut state: State, commands: &[Command]) -> Option<Vec<Var>> {
    let N = commands.len();

    state.A = state.A.min(2);
    state.B = state.B.min(2);
    state.C = state.C.min(2);
    #[cfg(debug_assertions)]
    eprintln!("\x1B[33m{}\x1B[0m = {:?}", "state", state);

    // dp[i][s] = back
    // commands[..i] を処理した後、
    // 各変数の値が s の状態に遷移できるとき dp[i][s] が存在し、
    // i = 0 なら back = None
    // i > 0 なら直前のコマンドで増加させた変数を up、遷移元の状態を s' として、
    // back = Some((up, s'))

    let mut dp = vec![HashMap::new(); N + 1];
    dp[0].insert(state, None);

    for i in 0..N {
        let command = commands[i];
        let (u, v, w) = command.vars();

        for un in 0..7 {
            for vn in 0..7 {
                if un == 0 && vn == 0 {
                    continue;
                }

                let mut state = State::default();
                *state.get_mut(u) = un;
                *state.get_mut(v) = vn;

                for wn in 0..7 {
                    *state.get_mut(w) = wn;

                    if !dp[i].contains_key(&state) {
                        continue;
                    }

                    for &(up, down) in &[(u, v), (v, u)] {
                        if state.get(down) == 0 {
                            continue;
                        }

                        let mut next = state;
                        *next.get_mut(up) += 1;
                        *next.get_mut(down) -= 1;

                        dp[i + 1].insert(next, Some((up, state)));
                    }
                }
            }
        }
    }

    let mut vars = vec![Var::A; N];
    let mut i = N;
    let mut back = *dp[N].values().next()?;

    while i >= 1 {
        i -= 1;

        let (up, prev) = back.unwrap();
        vars[i] = up;
        state = prev;
        back = dp[i].get(&state).cloned().unwrap();
    }

    Some(vars)
}

fn main() {
    let mut scan = Scan::new();

    let N = scan.word::<usize>();
    let A = scan.word::<usize>();
    let B = scan.word::<usize>();
    let C = scan.word::<usize>();
    let s = scan.list::<String>(N);

    let state = State { A, B, C };
    let commands = s
        .into_iter()
        .map(|s| Command::parse(&s))
        .collect::<Vec<_>>();

    match solve(state, &commands) {
        Some(vars) => {
            println!("Yes");
            for i in 0..N {
                println!("{}", vars[i].as_str());
            }
        }
        None => {
            println!("No");
        }
    }
}
