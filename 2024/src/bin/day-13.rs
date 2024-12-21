use std::collections::BTreeMap;

use aoc_2024::parse_input_file_raw;

#[derive(Clone)]
struct Machine {
    a: (u64, u64),
    b: (u64, u64),
    prize: (u64, u64),
}

fn parse_machine(lines: Vec<String>) -> Machine {
    let mut lines = lines.iter();
    let mut axy = lines
        .next()
        .unwrap()
        .strip_prefix("Button A: ")
        .unwrap()
        .split(", ")
        .collect::<Vec<_>>()
        .into_iter();
    let ax = axy
        .next()
        .unwrap()
        .strip_prefix("X+")
        .unwrap()
        .parse()
        .unwrap();
    let ay = axy
        .next()
        .unwrap()
        .strip_prefix("Y+")
        .unwrap()
        .parse()
        .unwrap();
    let mut bxy = lines
        .next()
        .unwrap()
        .strip_prefix("Button B: ")
        .unwrap()
        .split(", ")
        .collect::<Vec<_>>()
        .into_iter();
    let bx = bxy
        .next()
        .unwrap()
        .strip_prefix("X+")
        .unwrap()
        .parse()
        .unwrap();
    let by = bxy
        .next()
        .unwrap()
        .strip_prefix("Y+")
        .unwrap()
        .parse()
        .unwrap();
    let mut prizexy = lines
        .next()
        .unwrap()
        .strip_prefix("Prize: ")
        .unwrap()
        .split(", ")
        .collect::<Vec<_>>()
        .into_iter();
    let prizex = prizexy
        .next()
        .unwrap()
        .strip_prefix("X=")
        .unwrap()
        .parse()
        .unwrap();
    let prizey = prizexy
        .next()
        .unwrap()
        .strip_prefix("Y=")
        .unwrap()
        .parse()
        .unwrap();
    Machine {
        a: (ax, ay),
        b: (bx, by),
        prize: (prizex, prizey),
    }
}

fn play_game(
    mut visited: BTreeMap<(u64, u64), u64>,
    cost: u64,
    machine: &Machine,
    (x, y): (u64, u64),
) -> BTreeMap<(u64, u64), u64> {
    if let Some(old) = visited.get(&(x, y)) {
        if *old <= cost {
            return visited;
        }
    }
    visited.insert((x, y), cost);
    if x >= machine.prize.0 || y >= machine.prize.1 {
        visited
    } else {
        let visited = play_game(
            visited,
            cost + 3,
            machine,
            (x + machine.a.0, y + machine.a.1),
        );
        let visited = play_game(
            visited,
            cost + 1,
            machine,
            (x + machine.b.0, y + machine.b.1),
        );
        visited
    }
}

fn main() {
    let mut machine_lines = vec![];
    let mut machines = parse_input_file_raw(vec![], |mut acc, line| {
        if line.is_empty() {
            acc.push(parse_machine(machine_lines.clone()));
            machine_lines = vec![];
            acc
        } else {
            machine_lines.push(line);
            acc
        }
    });
    machines.push(parse_machine(machine_lines));

    let part_1 = machines
        .clone()
        .into_iter()
        .filter_map(|machine| {
            let tokens = play_game(BTreeMap::new(), 0, &machine, (0, 0))
                .get(&machine.prize)
                .cloned();
            // println!("{:?} => {:?}", machine.prize, tokens);
            tokens
        })
        .sum::<u64>();
    println!("Part 1: {part_1}");
}
