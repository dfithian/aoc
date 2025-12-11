use std::collections::{BTreeMap, BTreeSet, VecDeque};

use aoc_2025::parse_input_file;

fn main() {
    let devices = parse_input_file(BTreeMap::<String, Vec<String>>::new(), |mut acc, next| {
        let input = next[0..3].to_string();
        let outputs = next[5..].to_string().split(" ").map(|s| s.to_string()).collect::<Vec<String>>();
        acc.insert(input, outputs);
        acc
    });

    let mut routes = 0;
    let mut possibilities = VecDeque::from([("you".to_string(), BTreeSet::<String>::new())]);
    while let Some((device, seen)) = possibilities.pop_front() {
        if &device == "out" {
            routes += 1;
        } else if !seen.contains(&device) {
            devices.get(&device).cloned().unwrap_or(vec![]).into_iter().for_each(|next_device| {
                let mut next_seen = seen.clone();
                next_seen.insert(device.clone());
                possibilities.push_back((next_device, next_seen));
            });
        }
    }
    println!("part 1: {routes}");

    let mut routes = 0;
    let mut possibilities = VecDeque::from([("svr".to_string(), BTreeSet::<String>::new())]);
    while let Some((device, seen)) = possibilities.pop_front() {
        if &device == "out" {
            if seen.contains("dac") && seen.contains("fft") {
                routes += 1;
            }
        } else if !seen.contains(&device) {
            devices.get(&device).cloned().unwrap_or(vec![]).into_iter().for_each(|next_device| {
                let mut next_seen = seen.clone();
                next_seen.insert(device.clone());
                possibilities.push_back((next_device, next_seen));
            });
        }
        println!("{:?} {:?}", routes, possibilities.len());
    }
    println!("part 2: {routes}");
}
