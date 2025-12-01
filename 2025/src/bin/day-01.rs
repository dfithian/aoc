use aoc_2025::parse_input_file;

#[derive(Clone, Debug)]
enum Direction {
    L,
    R,
}

fn main() {
    let xs = parse_input_file(vec![], |mut xs, mut next| {
        let dist = next.split_off(1).parse::<u32>().unwrap();
        let dir = match next.as_str() {
            "L" => Direction::L,
            "R" => Direction::R,
            other => panic!("Unknown direction {other}"),
        };
        xs.push((dir, dist));
        xs
    });
    let mut pos = 50_u32;
    let n = xs.clone().into_iter().fold(0_u32, |n, (dir, dist)| {
        pos = match dir {
            Direction::L => (pos + 100 - (dist % 100)) % 100,
            Direction::R => (pos + (dist % 100)) % 100,
        };
        if pos == 0 { n + 1 } else { n }
    });
    println!("part 1: {n}");

    pos = 50;
    let n = xs.into_iter().fold(0_u32, |mut n, (dir, mut dist)| {
        n += dist / 100; // number of spins
        dist = dist % 100;
        let new_pos = match dir {
            Direction::L => {
                if dist > pos && pos != 0 { n += 1; }
                (pos + 100 - dist) % 100
            },
            Direction::R => {
                let new_pos = pos + dist;
                if new_pos > 100 { n += 1 };
                new_pos % 100
            },
        };
        if new_pos == 0 { n += 1; }
        pos = new_pos;
        n
    });
    println!("part 2: {n}");
}
