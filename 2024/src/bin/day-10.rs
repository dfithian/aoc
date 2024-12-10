use std::collections::{BTreeMap, BTreeSet, VecDeque};

use aoc_2024::parse_input_file;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Loc {
    i: i32,
    j: i32,
}

impl Loc {
    fn adjacents(&self) -> [Loc; 4] {
        let Loc { i, j } = self;
        [
            Loc { i: *i - 1, j: *j },
            Loc { i: *i + 1, j: *j },
            Loc { i: *i, j: *j - 1 },
            Loc { i: *i, j: *j + 1 },
        ]
    }
}

struct Trail {
    start: Loc,
    cur: (Loc, u32),
}

fn main() {
    let (_max_i, (_max_j, topo_map)) =
        parse_input_file((0, (0, BTreeMap::new())), |(i, (_, acc)), next| {
            (
                i + 1,
                next.chars().fold((0, acc), |(j, mut acc), c| {
                    acc.insert(Loc { i, j }, format!("{c}").parse::<u32>().unwrap());
                    (j + 1, acc)
                }),
            )
        });

    let mut stack = topo_map
        .clone()
        .into_iter()
        .filter_map(|(loc, n)| {
            if n == 0 {
                Some(Trail {
                    start: loc.clone(),
                    cur: (loc, 0),
                })
            } else {
                None
            }
        })
        .collect::<VecDeque<_>>();

    let mut trails = BTreeMap::new();
    let mut ratings = 0;
    while let Some(Trail { start, cur }) = stack.pop_back() {
        let n = cur.1;
        if n == 9 {
            ratings += 1;
            trails
                .entry(start)
                .and_modify(|peaks: &mut BTreeSet<Loc>| {
                    let _ = peaks.insert(cur.0.clone());
                })
                .or_insert(BTreeSet::from([cur.0]));
        } else {
            for new_pos in cur.0.adjacents() {
                if let Some(new_n) = topo_map.get(&new_pos) {
                    if *new_n == n + 1 {
                        stack.push_front(Trail {
                            start: start.clone(),
                            cur: (new_pos, *new_n),
                        });
                    }
                }
            }
        }
    }
    let score = trails
        .values()
        .map(|peaks| peaks.into_iter().count())
        .sum::<usize>();
    println!("part 1: {score}");
    println!("part 2: {ratings}");
}
