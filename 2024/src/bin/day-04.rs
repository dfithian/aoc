use std::collections::BTreeMap;
use strum::{EnumIter, IntoEnumIterator};

use aoc_2024::parse_input_file;

#[derive(EnumIter)]
enum Direction {
    N,
    E,
    S,
    W,
    NE,
    SE,
    SW,
    NW,
}

impl Direction {
    fn next(&self, pos: (i32, i32)) -> (i32, i32) {
        match self {
            Self::N => (pos.0 - 1, pos.1),
            Self::E => (pos.0, pos.1 + 1),
            Self::S => (pos.0 + 1, pos.1),
            Self::W => (pos.0, pos.1 - 1),
            Self::NE => (pos.0 - 1, pos.1 + 1),
            Self::SE => (pos.0 + 1, pos.1 + 1),
            Self::SW => (pos.0 + 1, pos.1 - 1),
            Self::NW => (pos.0 - 1, pos.1 - 1),
        }
    }
}

struct Map(BTreeMap<(i32, i32), char>);

impl Map {
    fn check(&self, dir: &Direction, mut word: Vec<char>, pos: (i32, i32)) -> bool {
        if let Some(c) = word.pop() {
            self.0.get(&pos) == Some(&c) && self.check(dir, word, dir.next(pos))
        } else {
            true
        }
    }

    fn xmas(&self, dir: Direction, pos: (i32, i32)) -> bool {
        self.check(&dir, vec!['S', 'A', 'M', 'X'], pos)
    }

    fn mas(&self, dir: Direction, pos: (i32, i32)) -> bool {
        self.check(&dir, vec!['S', 'A', 'M'], pos)
    }
}

fn main() {
    let map = Map(parse_input_file((0, BTreeMap::new()), |(i, acc), next| {
        (
            i + 1,
            next.chars().enumerate().fold(acc, |mut acc, (j, c)| {
                acc.insert((i as i32, j as i32), c);
                acc
            }),
        )
    })
    .1);

    let mut n = 0;
    for pos in map.0.keys() {
        for dir in Direction::iter() {
            if map.xmas(dir, *pos) {
                n += 1;
            }
        }
    }

    println!("part 1: {n}");

    let mut n = 0;
    for pos in map.0.keys() {
        if map.0.get(pos) == Some(&'A') {
            if map.mas(Direction::SE, Direction::NW.next(*pos))
                || map.mas(Direction::NW, Direction::SE.next(*pos))
            {
                if map.mas(Direction::SW, Direction::NE.next(*pos))
                    || map.mas(Direction::NE, Direction::SW.next(*pos))
                {
                    n += 1;
                }
            }
        }
    }

    println!("part 2: {n}");
}
