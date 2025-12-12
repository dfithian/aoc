use std::collections::{BTreeMap, BTreeSet, VecDeque};

use aoc_2025::parse_input_file;

fn walk(devices: &BTreeMap<String, BTreeSet<String>>, start: &'static str, end: &'static str, must_not_be_seen: BTreeSet<String>) -> u32 {
    let mut routes = 0;
    let mut possibilities = VecDeque::from([(start.to_string(), must_not_be_seen.clone())]);
    while let Some((device, seen)) = possibilities.pop_front() {
        if &device == end {
            routes += 1;
        } else if !seen.contains(&device) {
            devices.get(&device).cloned().unwrap_or(BTreeSet::new()).into_iter().for_each(|next_device| {
                let mut next_seen = seen.clone();
                next_seen.insert(device.clone());
                possibilities.push_back((next_device, next_seen));
            })
        }
    }
    routes
}

fn main() {
    let devices = parse_input_file(BTreeMap::<String, BTreeSet<String>>::new(), |mut acc, next| {
        let input = next[0..3].to_string();
        let outputs = next[5..].to_string().split(" ").map(|s| s.to_string()).collect::<BTreeSet<String>>();
        acc.insert(input, outputs);
        acc
    });

    let part1 = walk(&devices, "you", "out", BTreeSet::new());
    println!("part 1: {part1}");

    let mut devices = devices.into_iter().map(|(input, outputs)| {
        (
            input,
            outputs.into_iter().map(|output| (output, 1)).collect::<BTreeMap<String, u32>>(),
        )
    }).collect::<BTreeMap<String, BTreeMap<String, u32>>>();

    println!("{:?}", devices);
    let do_not_touch = BTreeSet::from(["svr".to_string(), "dac".to_string(), "fft".to_string(), "out".to_string()]);
    let mut devices_sorted = devices.clone().into_iter().filter(|(input, _)| !do_not_touch.contains(input)).collect::<Vec<(String, BTreeMap<String, u32>)>>();
    devices_sorted.sort_by_key(|(_, outputs)| outputs.len());

    for (input, _) in devices_sorted.into_iter() {
        let outputs = devices.get(&input).cloned().unwrap_or(BTreeMap::new());
        let upstream_inputs = devices.clone().into_iter().filter_map(|(other_input, other_outputs)| {
            if other_outputs.contains_key(&input) {
                Some(other_input)
            } else {
                None
            }
        }).collect::<Vec<String>>();
        println!("{:?}, {:?}, {:?}", input, outputs, upstream_inputs);
        if upstream_inputs.len() > 0 {
            devices.remove(&input);
        }
        for upstream_input in upstream_inputs.into_iter() {
            devices.entry(upstream_input.clone()).and_modify(|upstream_outputs| {
                outputs.clone().into_iter().for_each(|(output, n)| {
                    upstream_outputs.entry(output).and_modify(|m| *m += n).or_insert(n);
                });
                upstream_outputs.remove(&upstream_input);
                upstream_outputs.remove(&input);
            });
        }
        println!("{:?}", devices);
    }
    println!("{:?}", devices);

    let routes_svr = devices.get("svr").cloned().unwrap_or(BTreeMap::new());
    let routes_dac = devices.get("dac").cloned().unwrap_or(BTreeMap::new());
    let routes_fft = devices.get("fft").cloned().unwrap_or(BTreeMap::new());

    // let routes_svr_dac = routes_svr.get("dac").cloned().unwrap_or(0);
    // let routes_dac_fft = routes_dac.get("fft").cloned().unwrap_or(0);
    // let routes_fft_out = routes_fft.get("out").cloned().unwrap_or(0);
    let routes_svr_fft = routes_svr.get("fft").cloned().unwrap_or(0);
    let routes_fft_dac = routes_fft.get("dac").cloned().unwrap_or(0);
    let routes_dac_out = routes_dac.get("out").cloned().unwrap_or(0);

    // println!("svr -> dac {:?}", routes_svr_dac);
    // println!("dac -> fft {:?}", routes_dac_fft);
    // println!("fft -> out {:?}", routes_fft_out);
    println!("svr -> fft {:?}", routes_svr_fft);
    println!("fft -> dac {:?}", routes_fft_dac);
    println!("dac -> out {:?}", routes_dac_out);

    // let part2 = (routes_svr_dac * routes_dac_fft * routes_fft_out) + (routes_svr_fft * routes_fft_dac * routes_dac_out);
    let part2 = routes_svr_fft * routes_fft_dac * routes_dac_out;
    println!("part 2: {part2}");
}
