use std::collections::{BTreeMap, BTreeSet};

use aoc_2024::parse_input_file;

#[derive(Debug, Clone)]
enum Direction {
    N,
    E,
    S,
    W,
}

impl Direction {
    fn forward(&self, pos: (i32, i32)) -> (i32, i32) {
        match self {
            Self::N => (pos.0 - 1, pos.1),
            Self::E => (pos.0, pos.1 + 1),
            Self::S => (pos.0 + 1, pos.1),
            Self::W => (pos.0, pos.1 - 1),
        }
    }

    fn right(self) -> Self {
        match self {
            Self::N => Self::E,
            Self::E => Self::S,
            Self::S => Self::W,
            Self::W => Self::N,
        }
    }

    fn left(self) -> Self {
        match self {
            Self::N => Self::W,
            Self::E => Self::N,
            Self::S => Self::E,
            Self::W => Self::S,
        }
    }
}

#[derive(Debug, Clone)]
struct Reindeer {
    dir: Direction,
    pos: (i32, i32),
    cost: u64,
}

impl Reindeer {
    fn forward(self) -> Self {
        let pos = self.dir.forward(self.pos);
        let cost = self.cost + 1;
        Self {
            dir: self.dir,
            pos,
            cost,
        }
    }

    fn right(self) -> Self {
        Self {
            dir: self.dir.right(),
            pos: self.pos,
            cost: self.cost + 1000,
        }
        .forward()
    }

    fn left(self) -> Self {
        Self {
            dir: self.dir.left(),
            pos: self.pos,
            cost: self.cost + 1000,
        }
        .forward()
    }

    fn valid(&self, visits: &BTreeMap<(i32, i32), u64>, walls: &BTreeSet<(i32, i32)>) -> bool {
        if let Some(cost) = visits.get(&self.pos) {
            if *cost <= self.cost {
                return false;
            }
        }
        return !walls.contains(&self.pos);
    }
}

fn path(
    visits: &mut BTreeMap<(i32, i32), u64>,
    walls: &BTreeSet<(i32, i32)>,
    end: (i32, i32),
    cur: Reindeer,
) {
    visits.insert(cur.pos, cur.cost);
    if cur.pos != end {
        for x in [cur.clone().forward(), cur.clone().right(), cur.left()] {
            if x.valid(&visits, walls) {
                path(visits, walls, end, x);
            }
        }
    }
}

fn main() {
    let mut row = 0;
    let (walls, start, end) = parse_input_file((BTreeSet::new(), None, None), |mut acc, next| {
        let mut col = 0;
        acc = next.chars().fold(acc, |mut acc, c| {
            match c {
                '#' => {
                    let _ = acc.0.insert((row, col));
                }
                'S' => acc.1 = Some((row, col)),
                'E' => acc.2 = Some((row, col)),
                _ => {}
            }
            col += 1;
            acc
        });
        row += 1;
        acc
    });

    let start = start.unwrap();
    let end = end.unwrap();
    let mut visits = BTreeMap::new();
    path(
        &mut visits,
        &walls,
        end,
        Reindeer {
            dir: Direction::E,
            pos: start,
            cost: 0,
        },
    );
    let part_1 = visits.get(&end).unwrap();
    println!("Part 1: {part_1}");
}
