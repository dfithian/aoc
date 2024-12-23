use std::collections::{BTreeMap, BTreeSet};

use aoc_2024::parse_input_file;

fn walk(
    mut visits: BTreeMap<(i64, i64), u64>,
    corrupted: &BTreeSet<(i64, i64)>,
    end: (i64, i64),
    cost: u64,
    cur: (i64, i64),
) -> BTreeMap<(i64, i64), u64> {
    if let Some(old) = visits.get(&cur) {
        if *old <= cost {
            return visits;
        }
    }
    visits.insert(cur, cost);
    if end == cur {
        return visits;
    }
    vec![
        (cur.0 - 1, cur.1),
        (cur.0, cur.1 - 1),
        (cur.0 + 1, cur.1),
        (cur.0, cur.1 + 1),
    ]
    .into_iter()
    .filter(|pos| {
        pos.0 >= 0 && pos.0 <= end.0 && pos.1 >= 0 && pos.1 <= end.1 && !corrupted.contains(&pos)
    })
    .fold(visits, |vs, next| walk(vs, corrupted, end, cost + 1, next))
}

fn main() {
    let coordinates = parse_input_file(vec![], |mut acc, next| {
        let mut ps = next.split(",");
        acc.push((
            ps.next().unwrap().parse().unwrap(),
            ps.next().unwrap().parse().unwrap(),
        ));
        acc
    });

    // let end = (6, 6);
    // let i = 12;
    let end = (70, 70);
    let i = 1024;

    let corrupted = BTreeSet::from_iter(coordinates.iter().take(i.min(coordinates.len())).cloned());
    let visits = walk(BTreeMap::new(), &corrupted, end, 0, (0, 0));
    let part_1 = visits.get(&end).unwrap();
    println!("Part 1: {part_1}");

    let mut i = coordinates.len() - 1;
    loop {
        let corrupted = BTreeSet::from_iter(coordinates.iter().take(i).cloned());
        let visits = walk(BTreeMap::new(), &corrupted, end, 0, (0, 0));
        if visits.contains_key(&end) {
            break;
        }
        i -= 1;
    }
    let part_2 = coordinates[i];
    println!("Part 2: {:?}", part_2);
}
