use std::{
    collections::{BTreeSet, VecDeque},
    ops::BitXor,
};

use aoc_2024::parse_input_file;

// 2^6 = 64
// 2^5 = 32
// 2^11 = 2048
// 2^24 = 16777216
fn next_secret(x: &mut u64) -> i64 {
    let y = *x;
    *x = x.bitxor(*x * 64) % 16777216;
    *x = x.bitxor(*x / 32) % 16777216;
    *x = x.bitxor(*x * 2048) % 16777216;
    (*x % 10) as i64 - (y % 10) as i64
}

fn main() {
    let mut secrets = parse_input_file(vec![], |mut acc, next| {
        acc.push(next.parse::<u64>().unwrap());
        acc
    });

    let mut changes = vec![];
    for i in 0..secrets.len() {
        changes.push(vec![]);
        changes[i].push((0, secrets[i]));
    }
    for _ in 0..2000 {
        for i in 0..secrets.len() {
            let change = next_secret(&mut secrets[i]);
            changes[i].push((change, secrets[i] % 10));
        }
    }
    let part_1 = secrets.iter().sum::<u64>();
    println!("Part 1: {part_1}");

    let mut sequences = vec![];
    for i in 0..secrets.len() {
        println!("setting history for {i}");
        sequences.push(vec![]);
        let mut seq = VecDeque::new();
        for j in 0..changes[i].len() {
            seq.push_back(changes[i][j].0);
            if seq.len() > 4 {
                seq.pop_front();
            }
            if seq.len() == 4 {
                sequences[i].push((seq.clone(), changes[i][j].1));
            }
        }
    }

    let mut max_price_sum = 0;
    let mut max_price_seq = vec![];
    for seq in sequences.iter().fold(BTreeSet::new(), |acc_1, sequences_| {
        sequences_.iter().fold(acc_1, |mut acc, next| {
            acc.insert(next.0.clone());
            acc
        })
    }) {
        println!("checking seq {:?}", seq);
        let price_sum = sequences
            .iter()
            .map(|sequence_| {
                sequence_.iter().find_map(
                    |(seq_, price)| {
                        if seq_ == &seq {
                            Some(price)
                        } else {
                            None
                        }
                    },
                )
            })
            .map(|p| p.unwrap_or(&0))
            .sum::<u64>();
        if price_sum > max_price_sum {
            max_price_sum = price_sum;
            max_price_seq = seq.into();
        }
    }
    println!("Part 2: {:?} {:?}", max_price_sum, max_price_seq);
}
