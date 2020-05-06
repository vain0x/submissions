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

struct Solve {
    state: State,
    commands: Vec<Command>,
    vars: Vec<Var>,
}

impl Solve {
    fn new(state: State, commands: Vec<Command>) -> Self {
        Solve {
            state,
            commands,
            vars: vec![],
        }
    }

    fn solve(mut self) -> Option<Vec<Var>> {
        if self.dfs(0) {
            Some(self.vars)
        } else {
            None
        }
    }

    fn dfs(&mut self, i: usize) -> bool {
        let N = self.commands.len();
        if i == N {
            return true;
        }

        let (u, v, _) = self.commands[i].vars();

        // 両方試す。うまくいかない経路の探索は O(1) で終了する (false になる) ので、全体として O(N) に収まる。
        for &(up, down) in &[(u, v), (v, u)] {
            if self.state.get(down) >= 1 {
                self.vars.push(up);
                *self.state.get_mut(up) += 1;
                *self.state.get_mut(down) -= 1;

                if self.dfs(i + 1) {
                    return true;
                }

                self.vars.pop();
                *self.state.get_mut(up) -= 1;
                *self.state.get_mut(down) += 1;
            }
        }

        false
    }
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

    match Solve::new(state, commands).solve() {
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
