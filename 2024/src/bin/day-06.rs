use std::collections::{BTreeMap, BTreeSet};

use aoc_2024::parse_input_file;

fn main() {
    let (max_i, (max_j, (pos, grid))): (i32, (i32, ((i32, i32), BTreeSet<(i32, i32)>))) =
        parse_input_file(
            (0, (0, ((0, 0), BTreeSet::new()))),
            |(i, (_, (pos, grid))), next| {
                (
                    i + 1,
                    next.chars()
                        .fold((0, (pos, grid)), |(j, (mut pos, mut grid)), c| {
                            match c {
                                '^' => pos = (i, j),
                                '#' => {
                                    let _ = grid.insert((i, j));
                                }
                                '.' => {}
                                _ => panic!("unrecognized character {c}"),
                            }
                            (j + 1, (pos, grid))
                        }),
                )
            },
        );

    #[derive(Debug, Clone, PartialEq, Eq)]
    enum Direction {
        N,
        E,
        S,
        W,
    }

    impl Direction {
        fn right(&self) -> Self {
            match self {
                Self::N => Self::E,
                Self::E => Self::S,
                Self::S => Self::W,
                Self::W => Self::N,
            }
        }

        fn next(&self, (i, j): (i32, i32)) -> (i32, i32) {
            match self {
                Self::N => (i - 1, j),
                Self::E => (i, j + 1),
                Self::S => (i + 1, j),
                Self::W => (i, j - 1),
            }
        }

        fn prev(&self, (i, j): (i32, i32)) -> (i32, i32) {
            match self {
                Self::N => (i + 1, j),
                Self::E => (i, j - 1),
                Self::S => (i - 1, j),
                Self::W => (i, j + 1),
            }
        }
    }

    let in_bounds = |(i, j)| !(i < 0 || j < 0 || i >= max_i || j >= max_j);

    let mut dir = Direction::N;
    let mut visits = BTreeSet::new();
    let mut cur = pos;
    while in_bounds(cur) {
        if grid.contains(&cur) {
            let prev = dir.prev(cur);
            dir = dir.right();
            cur = dir.next(prev);
        } else {
            visits.insert(cur);
            cur = dir.next(cur);
        }
    }
    println!("part 1: {}", visits.into_iter().count());

    let mut dir = Direction::N;
    let mut visits = BTreeMap::new();
    let mut obstructions = BTreeSet::new();
    let mut cur = pos;
    while in_bounds(cur) {
        if grid.contains(&cur) {
            let prev = dir.prev(cur);
            dir = dir.right();
            cur = dir.next(prev);
        } else {
            let next = dir.next(cur);
            let if_i_were_to_turn_here = dir.right();
            let mut if_i_were_to_walk_straight = if_i_were_to_turn_here.next(cur);
            while in_bounds(if_i_were_to_walk_straight) && !grid.contains(&if_i_were_to_walk_straight) {
                if visits.get(&if_i_were_to_walk_straight) == Some(&if_i_were_to_turn_here) {
                    obstructions.insert(next);
                    break;
                }
                if_i_were_to_walk_straight = if_i_were_to_turn_here.next(if_i_were_to_walk_straight);
            }
            visits.insert(cur, dir.clone());
            cur = next;
        }
    }
    println!("part 2: {}", obstructions.into_iter().count());
}
