use std::collections::{BTreeMap, BTreeSet};

use aoc_2024::parse_input_file;

fn main() {
    let (max_i, (max_j, antennae)) =
        parse_input_file((0, (0, BTreeMap::new())), |(i, (_, acc)), next| {
            (i + 1, {
                next.chars().fold((0, acc), |(j, mut acc), c| {
                    if c != '.' {
                        let pos = (i, j);
                        acc.entry(c)
                            .and_modify(|poss: &mut Vec<(i32, i32)>| poss.push(pos))
                            .or_insert(vec![pos]);
                    }
                    (j + 1, acc)
                })
            })
        });

    let in_bounds = |(i, j)| i >= 0 && j >= 0 && i < max_i && j < max_j;
    let get_slope = |(i_1, j_1): (i32, i32), (i_2, j_2): (i32, i32)| (i_2 - i_1, j_2 - j_1);
    let get_ordered = |(i_1, j_1): (i32, i32), (i_2, j_2): (i32, i32)| {
        (i_1.min(i_2), i_1.max(i_2), j_1.min(j_2), j_1.max(j_2))
    };

    let mut antinodes = BTreeSet::new();
    for (_, poss) in antennae.iter() {
        for idx_1 in 0..poss.len() as i32 {
            for idx_2 in idx_1 + 1..poss.len() as i32 {
                let pos_1 = poss[idx_1 as usize];
                let pos_2 = poss[idx_2 as usize];
                let (i_diff, j_diff) = get_slope(pos_1, pos_2);
                let (i_min, i_max, j_min, j_max) = get_ordered(pos_1, pos_2);
                let is = [i_min - i_diff, i_max + i_diff];
                let js = if i_diff * j_diff > 0 {
                    [j_min - j_diff, j_max + j_diff]
                } else {
                    [j_max - j_diff, j_min + j_diff]
                };
                for pos in is.into_iter().zip(js) {
                    if in_bounds(pos) {
                        antinodes.insert(pos);
                    }
                }
            }
        }
    }
    println!("part 1: {}", antinodes.iter().count());

    let mut antinodes = BTreeSet::new();
    for (_, poss) in antennae.iter() {
        for idx_1 in 0..poss.len() as i32 {
            for idx_2 in idx_1 + 1..poss.len() as i32 {
                let pos_1 = poss[idx_1 as usize];
                let pos_2 = poss[idx_2 as usize];
                let (i_diff, j_diff) = get_slope(pos_1, pos_2);
                let (i_min, i_max, j_min, j_max) = get_ordered(pos_1, pos_2);
                if i_diff * j_diff > 0 {
                    let mut pos = (i_min, j_min);
                    while in_bounds(pos) {
                        antinodes.insert(pos);
                        pos.0 -= i_diff;
                        pos.1 -= j_diff;
                    }
                    pos = (i_max, j_max);
                    while in_bounds(pos) {
                        antinodes.insert(pos);
                        pos.0 += i_diff;
                        pos.1 += j_diff;
                    }
                } else {
                    let mut pos = (i_min, j_max);
                    while in_bounds(pos) {
                        antinodes.insert(pos);
                        pos.0 -= i_diff;
                        pos.1 -= j_diff;
                    }
                    pos = (i_max, j_min);
                    while in_bounds(pos) {
                        antinodes.insert(pos);
                        pos.0 += i_diff;
                        pos.1 += j_diff;
                    }
                }
            }
        }
    }
    println!("part 2: {}", antinodes.iter().count());
}
