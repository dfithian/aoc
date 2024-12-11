use std::collections::VecDeque;

use aoc_2024::parse_input_file;

fn main() {
    let mut stones = parse_input_file(VecDeque::new(), |mut acc, next| {
        acc.extend(next.split(" ").map(|s| s.parse::<u64>().unwrap()));
        acc
    });

    let mut i = 0;
    while i < 25 {
        stones = stones.into_iter().fold(VecDeque::new(), |mut acc, next| {
            let str = next.to_string();
            if next == 0 {
                acc.push_back(1);
            } else if str.len() % 2 == 0 {
                let (left, right) = str.split_at(str.len() / 2);
                acc.push_back(left.parse::<u64>().unwrap());
                acc.push_back(right.parse::<u64>().unwrap());
            } else {
                acc.push_back(2024 * next);
            }
            acc
        });
        i += 1;
    }
    println!("part 1: {:?}", stones.len());
}
