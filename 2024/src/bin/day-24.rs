use std::collections::{BTreeMap, VecDeque};

use aoc_2024::parse_input_file_raw;

enum Input {
    Base,
    Connections,
}

impl Input {
    fn parse(&mut self, net: &mut Net, next: String) {
        match self {
            Self::Base => {
                if next.is_empty() {
                    *self = Self::Connections;
                } else {
                    let mut next = next.split(": ");
                    let reg = next.next().unwrap();
                    let val = match next.next().unwrap() {
                        "0" => false,
                        "1" => true,
                        _ => panic!("oh no"),
                    };
                    net.inputs.insert(reg.to_string(), val);
                }
            }
            Self::Connections => {
                let mut next = next.split(" ");
                let lhs = next.next().unwrap();
                let gate = match next.next().unwrap() {
                    "AND" => Gate::And,
                    "OR" => Gate::Or,
                    "XOR" => Gate::Xor,
                    _ => panic!("oh no"),
                };
                let rhs = next.next().unwrap();
                let _ = next.next();
                let out = next.next().unwrap();
                net.gates
                    .push_back((lhs.to_string(), gate, rhs.to_string(), out.to_string()));
            }
        }
    }
}

#[derive(Debug)]
enum Gate {
    And,
    Or,
    Xor,
}

struct Net {
    inputs: BTreeMap<String, bool>,
    gates: VecDeque<(String, Gate, String, String)>,
}

fn eval(inputs: &mut BTreeMap<String, bool>, gates: &mut VecDeque<(String, Gate, String, String)>) {
    while let Some(gate) = gates.pop_front() {
        match (inputs.get(&gate.0), inputs.get(&gate.2)) {
            (Some(lhs_val), Some(rhs_val)) => {
                let val = match gate.1 {
                    Gate::And => *lhs_val && *rhs_val,
                    Gate::Or => *lhs_val || *rhs_val,
                    Gate::Xor => lhs_val != rhs_val,
                };
                inputs.insert(gate.3, val);
            }
            _ => gates.push_back(gate),
        }
    }
}

fn extract(inputs: &BTreeMap<String, bool>) -> u64 {
    inputs
        .iter()
        .filter_map(|(k, b)| {
            if let Some(k) = k.strip_prefix('z') {
                if *b {
                    return Some(2_u64.pow(k.parse::<u32>().unwrap()));
                }
            }
            None
        })
        .sum()
}

fn main() {
    let mut input = Input::Base;
    let mut net = Net {
        inputs: BTreeMap::new(),
        gates: VecDeque::new(),
    };
    parse_input_file_raw((), |_, next| input.parse(&mut net, next));

    eval(&mut net.inputs, &mut net.gates);
    let part_1 = extract(&net.inputs);
    println!("Part 1: {part_1}");
}
