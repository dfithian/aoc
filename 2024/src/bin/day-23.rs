use std::collections::BTreeSet;

use aoc_2024::parse_input_file;

fn main() {
    let connections = parse_input_file(BTreeSet::new(), |mut acc, next| {
        let mut computers = next.split("-");
        let x = computers.next().unwrap().to_string();
        let y = computers.next().unwrap().to_string();
        acc.insert(BTreeSet::from([x, y]));
        acc
    });

    let all_computers = connections.iter().fold(BTreeSet::new(), |mut acc, conn| {
        acc.extend(conn);
        acc
    });

    let mut triples_with_t = BTreeSet::new();
    for computer in all_computers.iter().filter(|s| s.starts_with('t')) {
        let these_connections = connections
            .iter()
            .filter(|conn| conn.contains(*computer))
            .flat_map(|conn| conn)
            .filter(|c| c != computer)
            .collect::<Vec<_>>();
        for i in 0..these_connections.len() {
            for j in (i + 1)..these_connections.len() {
                if connections.contains(&BTreeSet::from([
                    these_connections[i].to_string(),
                    these_connections[j].to_string(),
                ])) {
                    triples_with_t.insert(BTreeSet::from([
                        computer,
                        these_connections[i],
                        these_connections[j],
                    ]));
                }
            }
        }
    }

    let part_1 = triples_with_t.into_iter().count();
    println!("Part 1: {part_1}");
}
