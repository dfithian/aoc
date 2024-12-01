use std::collections::{BTreeMap, BTreeSet};
use std::io::Result;

use aoc_2024::parse_input_file;

fn main() -> Result<()> {
    let (mut xs, mut ys) = parse_input_file((vec![], vec![]), |(mut xs, mut ys), next| {
        if !next.is_empty() {
            let mut parts = next.split("   ");
            xs.push(parts.next().unwrap().parse::<u32>().unwrap());
            ys.push(parts.next().unwrap().parse::<u32>().unwrap());
        }
        (xs, ys)
    });
    xs.sort();
    ys.sort();

    let diffs = xs
        .iter()
        .zip(ys.iter())
        .map(|(x, y)| x.abs_diff(*y))
        .sum::<u32>();
    println!("part 1: {diffs}");

    let xs = xs.into_iter().fold(BTreeMap::new(), |mut acc, next| {
        acc.entry(next).and_modify(|n| *n = *n + 1).or_insert(1);
        acc
    });
    let ys = ys.into_iter().fold(BTreeMap::new(), |mut acc, next| {
        acc.entry(next).and_modify(|n| *n = *n + 1).or_insert(1);
        acc
    });
    let similar = xs
        .keys()
        .collect::<BTreeSet<_>>()
        .intersection(&ys.keys().collect())
        .fold(0, |acc, next| {
            let score = *next * xs.get(*next).unwrap_or(&1) * ys.get(*next).unwrap_or(&1);
            acc + score
        });
    println!("part 2: {similar}");

    Ok(())
}
