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

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
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

    fn vars(self) -> (Var, Var) {
        match self {
            Command::AB => (Var::A, Var::B),
            Command::AC => (Var::A, Var::C),
            Command::BC => (Var::B, Var::C),
        }
    }
}

#[derive(Clone, Copy)]
struct State {
    A: i64,
    B: i64,
    C: i64,
}

impl State {
    fn get(self, v: Var) -> i64 {
        match v {
            Var::A => self.A,
            Var::B => self.B,
            Var::C => self.C,
        }
    }

    fn get_mut(&mut self, v: Var) -> &mut i64 {
        match v {
            Var::A => &mut self.A,
            Var::B => &mut self.B,
            Var::C => &mut self.C,
        }
    }
}

fn solve_1(mut current: Var, commands: &[Command]) -> Option<Vec<Var>> {
    let mut vars = vec![];

    for command in commands {
        let up = match (command, current) {
            (Command::AB, Var::C) | (Command::AC, Var::B) | (Command::BC, Var::A) => return None,
            (Command::AB, Var::B) | (Command::AC, Var::C) => Var::A,
            (Command::AB, Var::A) | (Command::BC, Var::C) => Var::B,
            (Command::AC, Var::A) | (Command::BC, Var::B) => Var::C,
        };

        vars.push(up);
        current = up;
    }

    Some(vars)
}

fn solve(mut state: State, commands: &[Command]) -> Option<Vec<Var>> {
    match state {
        State { A: 0, B: 0, C: 0 } => return None,
        State { A: 1, B: 0, C: 0 } => return solve_1(Var::A, &commands),
        State { A: 0, B: 1, C: 0 } => return solve_1(Var::B, &commands),
        State { A: 0, B: 0, C: 1 } => return solve_1(Var::C, &commands),
        _ => {}
    }

    let N = commands.len();
    let mut vars = vec![];

    for i in 0..N {
        let command = commands[i];
        let (u, v) = command.vars();

        let (up, down) = match (state.get(u), state.get(v)) {
            (0, 0) => return None,
            (0, _) => (u, v),
            (_, 0) => (v, u),
            _ if i + 1 < N => match (u, commands[i + 1]) {
                (Var::A, Command::AB)
                | (Var::A, Command::AC)
                | (Var::B, Command::AB)
                | (Var::B, Command::BC)
                | (Var::C, Command::AC)
                | (Var::C, Command::BC) => (u, v),
                _ => (v, u),
            },
            _ => (u, v),
        };

        vars.push(up);
        *state.get_mut(up) += 1;
        *state.get_mut(down) += 1;

        debug_assert!(state.A >= 0 && state.B >= 0 && state.C >= 0);
    }

    Some(vars)
}

fn main() {
    let mut scan = Scan::new();

    let N = scan.word::<usize>();
    let A = scan.word::<i64>();
    let B = scan.word::<i64>();
    let C = scan.word::<i64>();
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
