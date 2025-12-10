use std::collections::VecDeque;

use aoc_2025::parse_input_file;

fn matches_1(cur: &[bool], desired: &[bool]) -> bool {
    for i in 0..cur.len() {
        let i = i as usize;
        if cur[i] != desired[i] {
            return false;
        }
    }
    return true;
}

enum Match2 {
    NotValid,
    Valid,
    NeverAgainValid,
}

fn matches_2(cur: &[u32], desired: &[u32]) -> Match2 {
    let mut invalid = false;
    for i in 0..cur.len() {
        let i = i as usize;
        if cur[i] > desired[i] {
            return Match2::NeverAgainValid;
        } else if cur[i] < desired[i] {
            invalid = true;
        }
    }
    if invalid {
        return Match2::NotValid;
    }
    return Match2::Valid;
}

struct Machine {
    desired: Vec<bool>,
    toggles: Vec<Vec<usize>>,
    joltage: Vec<u32>,
}

impl Machine {
    fn run_1(self, i: usize) -> u32 {
        let mut min = u32::MAX;
        let mut possibilities = self.toggles.clone().into_iter().map(|ts| (1, vec![false; self.desired.len()], ts)).collect::<VecDeque<(u32, Vec<bool>, Vec<usize>)>>();
        while let Some((n, mut cur, toggle)) = possibilities.pop_front() {
            for i in toggle.iter() {
                cur[*i] = !cur[*i];
            }
            if matches_1(&cur, &self.desired) {
                min = min.min(n);
                possibilities = possibilities.into_iter().filter(|(m, _, _)| *m < min).collect::<VecDeque<(u32, Vec<bool>, Vec<usize>)>>();
            } else {
                for toggle in self.toggles.clone().into_iter() {
                    possibilities.push_back((n + 1, cur.clone(), toggle));
                }
            }
        }
        println!("index {:02} min {min} for {:?}", i, self.desired);
        return min;
    }

    fn run_2(self, i: usize) -> u32 {
        let mut min = u32::MAX;
        let mut possibilities = self.toggles.clone().into_iter().map(|ts| (1, vec![0; self.desired.len()], ts)).collect::<VecDeque<(u32, Vec<u32>, Vec<usize>)>>();
        while let Some((n, mut cur, toggle)) = possibilities.pop_front() {
            for i in toggle.iter() {
                cur[*i] += 1;
            }
            match matches_2(&cur, &self.joltage) {
                Match2::NotValid => for toggle in self.toggles.clone().into_iter() {
                    possibilities.push_back((n + 1, cur.clone(), toggle));
                },
                Match2::Valid => {
                    min = min.min(n);
                    possibilities = possibilities.into_iter().filter(|(m, _, _)| *m < min).collect::<VecDeque<(u32, Vec<u32>, Vec<usize>)>>();
                },
                Match2::NeverAgainValid => (),
            }
        }
        println!("index {:02} min {min} for {:?}", i, self.joltage);
        return min;
    }
}

fn main() {
    let machines = parse_input_file(vec![], |mut acc, next| {
        let mut desired = vec![];
        let mut toggles = vec![];
        let mut joltage = vec![];
        let mut words = next.split(" ");
        while let Some(word) = words.next() {
            match word.chars().next().unwrap() {
                '[' =>
                    desired = word.trim_matches(|c| c == '[' || c == ']').chars().map(|c| match c {
                        '.' => false,
                        '#' => true,
                        _ => panic!("unknown word {word}"),
                    }).collect::<Vec<bool>>(),
                '(' =>
                    toggles.push(word.trim_matches(|c| c == '(' || c == ')').split(",").map(|s| s.parse::<usize>().unwrap()).collect::<Vec<usize>>()),
                '{' =>
                    joltage = word.trim_matches(|c| c == '{' || c == '}').split(",").map(|s| s.parse::<u32>().unwrap()).collect::<Vec<u32>>(),
                _ => panic!("unknown word {word}"),
            }
        }
        acc.push(Machine {
            desired,
            toggles,
            joltage,
        });
        acc
    });

    // let part1 = machines.clone().into_iter().enumerate().map(|(i, m)| m.run_1(i)).sum::<u32>();
    // println!("part 1: {part1}");

    let part2 = machines.into_iter().enumerate().map(|(i, m)| m.run_2(i)).sum::<u32>();
    println!("part 2: {part2}");
}
