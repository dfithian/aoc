use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{anychar, digit1},
    combinator::{map, map_res, recognize},
    multi::many0,
    sequence::{delimited, pair, tuple},
    IResult,
};

use aoc_2024::parse_input_file;

#[derive(Debug)]
enum Command {
    Mul(u32, u32),
    Do,
    Dont,
}

fn u32(input: &str) -> IResult<&str, u32> {
    map_res(recognize(digit1), str::parse)(input)
}

fn mul(input: &str) -> IResult<&str, Command> {
    map(
        pair(
            tag("mul"),
            delimited(tag("("), tuple((u32, tag(","), u32)), tag(")")),
        ),
        |(_, (x, _, y))| Command::Mul(x, y),
    )(input)
}

fn do_(input: &str) -> IResult<&str, Command> {
    map(tag("do()"), |_| Command::Do)(input)
}

fn dont(input: &str) -> IResult<&str, Command> {
    map(tag("don't()"), |_| Command::Dont)(input)
}

fn cmd(input: &str) -> IResult<&str, Command> {
    alt((mul, do_, dont))(input)
}

fn main() {
    let instructions = parse_input_file(String::new(), |acc, next| format!("{acc}{next}"));

    let commands = many0(alt((map(cmd, Some), map(anychar, |_| None))))(instructions.as_str())
        .unwrap()
        .1;

    let sum_of_products = commands.iter().flatten().fold(0, |acc, next| match next {
        Command::Mul(x, y) => acc + (x * y),
        Command::Do => acc,
        Command::Dont => acc,
    });
    println!("part 1: {sum_of_products}");

    let sum_of_products = commands
        .iter()
        .flatten()
        .fold((true, 0), |(should, acc), next| match next {
            Command::Mul(x, y) => (should, if should { acc + (x * y) } else { acc }),
            Command::Do => (true, acc),
            Command::Dont => (false, acc),
        })
        .1;
    println!("part 2: {sum_of_products}");
}
