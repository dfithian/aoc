use aoc_2024::parse_input_file_raw;

#[derive(Clone)]
struct Machine {
    a: (i64, i64),
    b: (i64, i64),
    prize: (i64, i64),
}

fn parse_button(name: &str, line: String) -> (i64, i64) {
    let mut xy = line
        .strip_prefix(&format!("Button {name}: "))
        .map(|x| x.split(", ").collect::<Vec<_>>())
        .unwrap_or(vec![])
        .into_iter();
    let x = xy
        .next()
        .and_then(|x| x.strip_prefix("X+"))
        .and_then(|x| x.parse::<i64>().ok())
        .unwrap();
    let y = xy
        .next()
        .and_then(|y| y.strip_prefix("Y+"))
        .and_then(|y| y.parse::<i64>().ok())
        .unwrap();
    (x, y)
}

fn parse_prize(line: String) -> (i64, i64) {
    let mut xy = line
        .strip_prefix("Prize: ")
        .map(|x| x.split(", ").collect::<Vec<_>>())
        .unwrap_or(vec![])
        .into_iter();
    let x = xy
        .next()
        .and_then(|x| x.strip_prefix("X="))
        .and_then(|x| x.parse::<i64>().ok())
        .unwrap();
    let y = xy
        .next()
        .and_then(|y| y.strip_prefix("Y="))
        .and_then(|y| y.parse::<i64>().ok())
        .unwrap();
    (x, y)
}

fn parse_machine(lines: Vec<String>) -> Machine {
    let mut lines = lines.iter();
    let a = parse_button("A", lines.next().unwrap().to_string());
    let b = parse_button("B", lines.next().unwrap().to_string());
    let prize = parse_prize(lines.next().unwrap().to_string());
    Machine { a, b, prize }
}

/// x*a1 + y*a2 = A
/// x = (A - y*a2)/a1
///
/// x*b1 + y*b2 = B
/// (A - y*a2)*b1/a1 + y*b2 = B
/// y = (a1*B - A*b1)/(a1*b2 - a2*b1)
fn solve(Machine { a, b, prize }: &Machine) -> i64 {
    // let y = (a.0 * prize.1 - prize.0 * b.0) / (a.0 * b.1 - a.1 * b.0);
    // let x = (prize.0 - y * a.1) / a.0;
    if (a.0 * prize.1 - prize.0 * a.1) % (a.0 * b.1 - b.0 * a.1) != 0 {
        return 0;
    }
    let y = (a.0 * prize.1 - prize.0 * a.1) / (a.0 * b.1 - b.0 * a.1);
    if (prize.0 - y * b.0) % a.0 != 0 {
        return 0;
    }
    let x = (prize.0 - y * b.0) / a.0;
    3 * x + y
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

    let part_1 = machines.iter().map(solve).sum::<i64>();
    println!("Part 1: {part_1}");

    let part_2 = machines
        .into_iter()
        .map(|mut m| {
            m.prize.0 += 10000000000000;
            m.prize.1 += 10000000000000;
            solve(&m)
        })
        .sum::<i64>();
    println!("Part 2: {part_2}");
}
