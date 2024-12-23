use std::collections::BTreeSet;

use aoc_2024::parse_input_file_raw;

enum Input {
    Patterns,
    Blank,
    Designs,
}

impl Input {
    fn parse(&mut self, patterns: &mut BTreeSet<String>, designs: &mut Vec<String>, line: String) {
        match self {
            Self::Patterns => {
                *patterns = line.split(", ").map(|s| s.to_string()).collect();
                *self = Self::Blank;
            }
            Self::Blank => *self = Self::Designs,
            Self::Designs => {
                designs.push(line);
            }
        }
    }
}

fn check(
    patterns: &BTreeSet<String>,
    checked: BTreeSet<String>,
    prefix: String,
    suffix: String,
) -> (bool, BTreeSet<String>) {
    if suffix.is_empty() {
        return (true, checked);
    }
    patterns
        .iter()
        .fold((false, checked), |(is_valid, mut acc), infix| {
            if !is_valid {
                if let Some(new_suffix) = suffix.strip_prefix(infix) {
                    let new_prefix = format!("{prefix}{infix}");
                    if !acc.contains(&new_prefix) {
                        acc.insert(new_prefix.clone());
                        return check(patterns, acc, new_prefix, new_suffix.to_string());
                    }
                }
            }
            (is_valid, acc)
        })
}

fn main() {
    let mut input = Input::Patterns;
    let mut patterns = BTreeSet::new();
    let mut designs = vec![];
    parse_input_file_raw((), |_, next| input.parse(&mut patterns, &mut designs, next));

    let part_1 = designs
        .iter()
        .filter(|design| {
            check(
                &patterns,
                BTreeSet::new(),
                "".to_string(),
                design.to_string(),
            )
            .0
        })
        .count();
    println!("Part 1: {part_1}");
}
