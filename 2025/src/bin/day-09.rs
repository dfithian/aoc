use aoc_2025::parse_input_file;

fn area(p1: (u64, u64), p2: (u64, u64)) -> u64 {
    (p1.0.max(p2.0) + 1 - p1.0.min(p2.0)) * (p1.1.max(p2.1) + 1 - p1.1.min(p2.1))
}

fn intersects((p1, p2): ((u64, u64), (u64, u64)), (l1, l2): ((u64, u64), (u64, u64))) -> bool {
    let p_row_min = p1.0.min(p2.0);
    let p_row_max = p1.0.max(p2.0);
    let p_col_min = p1.1.min(p2.1);
    let p_col_max = p1.1.max(p2.1);
    let l_row_min = l1.0.min(l2.0);
    let l_row_max = l1.0.max(l2.0);
    let l_col_min = l1.1.min(l2.1);
    let l_col_max = l1.1.max(l2.1);
    if l1.0 == l2.0 {
        p_row_min < l_row_min && l_row_min < p_row_max && (
            (l_col_min <= p_col_min && p_col_min < l_col_max) ||
                (l_col_min < p_col_max && p_col_max <= l_col_max)
        )
    } else if l1.1 == l2.1 {
        p_col_min < l_col_min && l_col_min < p_col_max && (
            (l_row_min <= p_row_min && p_row_min < l_row_max) ||
                (l_row_min < p_row_max && p_row_max <= l_row_max)
        )
    } else {
        false
    }
}

fn main() {
    let coordinates = parse_input_file(vec![], |mut acc, next| {
        let mut num_strs = next.split(",").into_iter();
        let x = num_strs.next().unwrap().parse::<u64>().unwrap();
        let y = num_strs.next().unwrap().parse::<u64>().unwrap();
        acc.push((x, y));
        acc
    });

    let segments = coordinates
        .clone()
        .into_iter()
        .enumerate()
        .flat_map(|(i, p1)| {
            let mut rest = coordinates.clone();
            rest.drain(0..=i);
            rest.into_iter().map(move |p2| (p1, p2))
        })
        .collect::<Vec<((u64, u64), (u64, u64))>>();

    let mut coordinates_with_areas = segments
        .clone()
        .into_iter()
        .map(|(p1, p2)| (p1, p2, area(p1, p2)))
        .collect::<Vec<((u64, u64), (u64, u64), u64)>>();
    coordinates_with_areas.sort_by_key(|(_, _, a)| *a);
    coordinates_with_areas.reverse();

    let part1 = coordinates_with_areas[0].2;
    println!("part 1: {part1}");

    for (p1, p2, a) in coordinates_with_areas.into_iter() {
        if !segments.clone().into_iter().any(|(l1, l2)| intersects((p1, p2), (l1, l2))) {
            println!("part 2: {a}");
            break;
        }
    }
}
