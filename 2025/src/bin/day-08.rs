use std::collections::BTreeMap;

use aoc_2025::parse_input_file;

fn distance(p1: (u64, u64, u64), p2: (u64, u64, u64)) -> u64 {
    (((p1.0 as i64 - p2.0 as i64).pow(2) + (p1.1 as i64 - p2.1 as i64).pow(2) + (p1.2 as i64 - p2.2 as i64).pow(2)) as f32).sqrt() as u64
}

fn main() {
    let coordinates = parse_input_file(vec![], |mut acc, next| {
        let mut num_strs = next.split(",").into_iter();
        let x = num_strs.next().unwrap().parse::<u64>().unwrap();
        let y = num_strs.next().unwrap().parse::<u64>().unwrap();
        let z = num_strs.next().unwrap().parse::<u64>().unwrap();
        acc.push((x, y, z));
        acc
    });

    let mut coordinates_by_least_distance = coordinates
        .clone()
        .into_iter()
        .enumerate()
        .flat_map(|(i, p1)| {
            let mut rest = coordinates.clone();
            rest.drain(0..=i);
            rest.into_iter().map(move |p2| (p1, p2))
        })
        .collect::<Vec<((u64, u64, u64), (u64, u64, u64))>>();
    coordinates_by_least_distance.sort_by_key(|(p1, p2)| distance(*p1, *p2));

    // part 1 mutations
    // coordinates_by_least_distance.drain(10..);
    // coordinates_by_least_distance.drain(1000..);

    coordinates_by_least_distance.reverse();

    let mut cur_id = 0_u64;
    let mut graph_ids = BTreeMap::<(u64, u64, u64), u64>::new();
    let mut circuit_size = BTreeMap::<u64, u64>::new();
    while let Some((p1, p2)) = coordinates_by_least_distance.pop() {
        match (graph_ids.get(&p1).cloned(), graph_ids.get(&p2).cloned()) {
            (Some(id1), Some(id2)) => if id1 != id2 {
                let mut new_size = 0;
                for k in graph_ids.clone().keys() {
                    graph_ids.entry(*k).and_modify(|id| if *id == id1 || *id == id2 { *id = cur_id; new_size += 1; });
                }
                circuit_size.remove(&id1);
                circuit_size.remove(&id2);
                circuit_size.insert(cur_id, new_size);
                cur_id += 1;
            }
            (Some(id), _) => {
                graph_ids.insert(p2, id);
                circuit_size.entry(id).and_modify(|s| *s += 1);
            },
            (_, Some(id)) => {
                graph_ids.insert(p1, id);
                circuit_size.entry(id).and_modify(|s| *s += 1);
            }
            _ => {
                graph_ids.insert(p1, cur_id);
                graph_ids.insert(p2, cur_id);
                circuit_size.insert(cur_id, 2);
                cur_id += 1;
            },
        }
        if circuit_size.get(&(cur_id - 1)).is_some_and(|c| *c == coordinates.len() as u64) {
            let part2 = p1.0 * p2.0;
            println!("part 2: {part2}");
            break;
        }
    }
    if circuit_size.len() >= 3 {
        let mut circuit_sizes = circuit_size.values().cloned().collect::<Vec<u64>>();
        circuit_sizes.sort();
        circuit_sizes.reverse();
        let part1 = circuit_sizes[0] * circuit_sizes[1] * circuit_sizes[2];
        println!("part 1: {part1}");
    }
}
