use std::collections::BTreeSet;

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
    println!("part 1: {}", visits.clone().into_iter().count());

    let max_visits = 10001; // if we visit more than the size of the grid, we're in a loop
    let mut dir = Direction::N;
    let mut obstructions = BTreeSet::new();
    let mut cur = pos;
    while in_bounds(cur) {
        if grid.contains(&cur) {
            let prev = dir.prev(cur);
            dir = dir.right();
            cur = dir.next(prev);
        } else {
            let next = dir.next(cur);
            if in_bounds(next) && !grid.contains(&next) {
                let mut lookahead_grid = grid.clone();
                lookahead_grid.insert(next);
                let mut lookahead_dir = dir.right();
                let mut lookahead_cur = lookahead_dir.next(cur);
                let mut lookahead_visits = 0;
                while in_bounds(lookahead_cur) {
                    if lookahead_grid.contains(&lookahead_cur) {
                        let prev = lookahead_dir.prev(lookahead_cur);
                        lookahead_dir = lookahead_dir.right();
                        lookahead_cur = lookahead_dir.next(prev);
                    } else {
                        if lookahead_visits > max_visits {
                            obstructions.insert(next);
                            break;
                        }
                        lookahead_visits += 1;
                        lookahead_cur = lookahead_dir.next(lookahead_cur);
                    }
                }
            }
            cur = next;
        }
    }
    println!("part 2: {}", obstructions.into_iter().count());
}
