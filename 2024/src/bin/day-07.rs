use aoc_2024::parse_input_file;

fn main() {
    let calibrations = parse_input_file(vec![], |mut acc, next| {
        let mut next = next.split(": ");
        let ans = next.next().unwrap().parse::<u64>().unwrap();
        let inputs = next
            .next()
            .unwrap()
            .split(" ")
            .map(|s| s.parse::<u64>().unwrap())
            .collect::<Vec<_>>();
        acc.push((ans, inputs));
        acc
    });

    fn valid(ans: u64, inputs: &Vec<u64>, acc: Option<u64>) -> bool {
        if Some(ans) < acc {
            return false;
        }
        let mut inputs = inputs.clone();
        if let Some(next) = inputs.pop() {
            valid(ans, &inputs, Some(acc.unwrap_or(1) * next))
                || valid(ans, &inputs, Some(acc.unwrap_or(0) + next))
        } else {
            Some(ans) == acc
        }
    }

    let mut n = 0;
    for (ans, mut inputs) in calibrations.clone().into_iter() {
        inputs.reverse();
        if valid(ans, &inputs, None) {
            n += ans;
        }
    }
    println!("part 1: {n}");

    fn valid_2(ans: u64, inputs: &Vec<u64>, acc: Option<u64>) -> bool {
        if Some(ans) < acc {
            return false;
        }
        let mut inputs = inputs.clone();
        if let Some(next) = inputs.pop() {
            valid_2(ans, &inputs, Some(acc.unwrap_or(1) * next))
                || valid_2(ans, &inputs, Some(acc.unwrap_or(0) + next))
                || valid_2(
                    ans,
                    &inputs,
                    Some(
                        format!("{}{}", acc.unwrap_or(0), next)
                            .parse::<u64>()
                            .unwrap(),
                    ),
                )
        } else {
            Some(ans) == acc
        }
    }

    let mut n = 0;
    for (ans, mut inputs) in calibrations.into_iter() {
        inputs.reverse();
        if valid_2(ans, &inputs, None) {
            n += ans;
        }
    }
    println!("part 2: {n}");
}
