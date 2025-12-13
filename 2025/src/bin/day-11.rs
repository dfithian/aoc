use std::collections::{BTreeMap, BTreeSet};

use aoc_2025::parse_input_file;

fn count_routes(devices: &mut BTreeMap<String, BTreeMap<String, u64>>, for_keys: BTreeSet<String>) {
    for input in devices.clone().keys().cloned().filter(|input| !for_keys.contains(input)) {
        let outputs = devices.get(&input).cloned().unwrap();
        devices.remove(&input);
        for (upstream_input, mut upstream_outputs) in devices.clone().into_iter().filter(|(_, other_outputs)| other_outputs.contains_key(&input)) {
            let input_count = upstream_outputs.remove(&input).unwrap();
            for (output, count) in outputs.clone().into_iter() {
                upstream_outputs.entry(output)
                    .and_modify(|m| *m += input_count * count)
                    .or_insert(input_count * count);
            }
            devices.insert(upstream_input, upstream_outputs);
        }
    }
}

fn main() {
    let devices = parse_input_file(BTreeMap::<String, BTreeMap<String, u64>>::new(), |mut acc, next| {
        let input = next[0..3].to_string();
        let outputs = next[5..].to_string().split(" ").map(|s| (s.to_string(), 1)).collect::<BTreeMap<String, u64>>();
        acc.insert(input, outputs);
        acc
    });

    let mut part1_devices = devices.clone();
    count_routes(&mut part1_devices, BTreeSet::from(["you".to_string(), "out".to_string()]));
    let part1 = part1_devices.get("you").cloned().unwrap_or(BTreeMap::new()).get("out").cloned().unwrap_or(0);
    println!("part 1: {part1}");

    let mut part2_devices = devices;
    count_routes(&mut part2_devices, BTreeSet::from(["svr".to_string(), "dac".to_string(), "fft".to_string(), "out".to_string()]));
    let routes_svr_fft = part2_devices.get("svr").cloned().unwrap_or(BTreeMap::new()).get("fft").cloned().unwrap_or(0);
    let routes_fft_dac = part2_devices.get("fft").cloned().unwrap_or(BTreeMap::new()).get("dac").cloned().unwrap_or(0);
    let routes_dac_out = part2_devices.get("dac").cloned().unwrap_or(BTreeMap::new()).get("out").cloned().unwrap_or(0);
    let part2 = routes_svr_fft * routes_fft_dac * routes_dac_out;
    println!("part 2: {part2}");
}
