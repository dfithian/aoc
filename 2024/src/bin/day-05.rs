use std::collections::{BTreeMap, BTreeSet};

use aoc_2024::parse_input_file_split;

fn main() {
    let (rules, input) = parse_input_file_split(
        (BTreeMap::new(), vec![]),
        |(mut rules, input), next| {
            let next = next
                .split("|")
                .map(|s| s.parse::<u32>().unwrap())
                .collect::<Vec<_>>();
            rules
                .entry(next[0])
                .and_modify(|xs: &mut BTreeSet<u32>| {
                    xs.insert(next[1]);
                })
                .or_insert(BTreeSet::from([next[1]]));
            (rules, input)
        },
        |(rules, mut input), next: String| {
            input.push(
                next.split(",")
                    .map(|n| n.parse::<u32>().unwrap())
                    .collect::<Vec<_>>(),
            );
            (rules, input)
        },
    );

    let is_valid = |pages: &Vec<u32>| {
        for i in 1..pages.len() {
            let before = pages[i];
            let afters = rules.get(&before).cloned().unwrap_or_default();
            for j in 0..i {
                if afters.contains(&pages[j]) {
                    return false;
                }
            }
        }
        return true;
    };

    let make_valid = |pages: Vec<u32>| {
        let mut new_pages = vec![pages[0]];
        for i in 1..pages.len() {
            let before = pages[i];
            let afters = rules.get(&before).cloned().unwrap_or_default();
            let mut reset = false;
            for j in 0..i {
                if afters.contains(&new_pages[j]) {
                    new_pages.insert(j, before);
                    reset = true;
                    break;
                }
            }
            if !reset {
                new_pages.push(before);
            }
        }
        new_pages
    };

    let n = input
        .clone()
        .into_iter()
        .filter(is_valid)
        .map(|pages| pages[pages.len() / 2])
        .sum::<u32>();
    println!("part 1: {n}");

    let n = input
        .into_iter()
        .filter(|pages| !is_valid(pages))
        .map(make_valid)
        .map(|pages| pages[pages.len() / 2])
        .sum::<u32>();
    println!("part 2: {n}");
}
