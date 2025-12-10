use aoc_2025::parse_input_file;

fn area(p1: (u64, u64), p2: (u64, u64)) -> u64 {
    (p1.0.max(p2.0) + 1 - p1.0.min(p2.0)) * (p1.1.max(p2.1) + 1 - p1.1.min(p2.1))
}

fn main() {
    let coordinates = parse_input_file(vec![], |mut acc, next| {
        let mut num_strs = next.split(",").into_iter();
        let x = num_strs.next().unwrap().parse::<u64>().unwrap();
        let y = num_strs.next().unwrap().parse::<u64>().unwrap();
        acc.push((x, y));
        acc
    });

    let mut coordinates_by_area = coordinates
        .clone()
        .into_iter()
        .enumerate()
        .flat_map(|(i, p1)| {
            let mut rest = coordinates.clone();
            rest.drain(0..=i);
            rest.into_iter().map(move |p2| (p1, p2))
        })
        .collect::<Vec<((u64, u64), (u64, u64))>>();
    coordinates_by_area.sort_by_key(|(p1, p2)| area(*p1, *p2));
    coordinates_by_area.reverse();

    let (p1, p2) = coordinates_by_area[0];
    let part1 = area(p1, p2);
    println!("part 1: {part1}");
}
