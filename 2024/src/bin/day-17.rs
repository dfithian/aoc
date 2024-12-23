use std::ops::BitXor;

use aoc_2024::parse_input_file_raw;

enum Input {
    A,
    B,
    C,
    Blank,
    Program,
}

impl Input {
    fn parse(&mut self, program: &mut Program, line: String) {
        match self {
            Self::A => {
                program.register.a = line
                    .chars()
                    .skip("Register A: ".len())
                    .collect::<String>()
                    .parse()
                    .unwrap();
                *self = Self::B;
            }
            Self::B => {
                program.register.b = line
                    .chars()
                    .skip("Register B: ".len())
                    .collect::<String>()
                    .parse()
                    .unwrap();
                *self = Self::C;
            }
            Self::C => {
                program.register.c = line
                    .chars()
                    .skip("Register C: ".len())
                    .collect::<String>()
                    .parse()
                    .unwrap();
                *self = Self::Blank;
            }
            Self::Blank => {
                *self = Self::Program;
            }
            Self::Program => {
                program.commands = line
                    .chars()
                    .skip("Program: ".len())
                    .collect::<String>()
                    .split(",")
                    .map(|c| c.parse::<i64>().unwrap())
                    .collect();
            }
        }
    }
}

#[derive(Debug)]
struct Register {
    a: i64,
    b: i64,
    c: i64,
}

impl Register {
    fn combo(&self, op: i64) -> i64 {
        match op {
            0 => 0,
            1 => 1,
            2 => 2,
            3 => 3,
            4 => self.a,
            5 => self.b,
            6 => self.c,
            _ => panic!("oh no"),
        }
    }

    fn run(&mut self, commands: Vec<i64>) -> Vec<i64> {
        let mut out = vec![];
        let mut i = 0;
        while i < commands.len() {
            match commands[i] {
                0 => {
                    self.a = self.a / 2_i64.pow(self.combo(commands[i + 1]) as u32);
                    i += 2;
                }
                1 => {
                    self.b = self.b.bitxor(commands[i + 1]);
                    i += 2;
                }
                2 => {
                    self.b = self.combo(commands[i + 1]) % 8;
                    i += 2;
                }
                3 => {
                    if self.a != 0 {
                        i = commands[i + 1] as usize;
                    } else {
                        i += 2;
                    }
                }
                4 => {
                    self.b = self.b.bitxor(self.c);
                    i += 2;
                }
                5 => {
                    out.push(self.combo(commands[i + 1]) % 8);
                    i += 2;
                }
                6 => {
                    self.b = self.a / 2_i64.pow(self.combo(commands[i + 1]) as u32);
                    i += 2;
                }
                7 => {
                    self.c = self.a / 2_i64.pow(self.combo(commands[i + 1]) as u32);
                    i += 2;
                }
                _ => panic!("oh no"),
            }
        }
        out
    }
}

struct Program {
    register: Register,
    commands: Vec<i64>,
}

fn main() {
    let mut input = Input::A;
    let mut program = parse_input_file_raw(
        Program {
            register: Register { a: 0, b: 0, c: 0 },
            commands: vec![],
        },
        |mut acc, next| {
            input.parse(&mut acc, next);
            acc
        },
    );
    let out = program.register.run(program.commands);
    let part_1 = out
        .into_iter()
        .map(|n| format!("{n}"))
        .collect::<Vec<_>>()
        .join(",");
    println!("Part 1: {part_1}");
}
