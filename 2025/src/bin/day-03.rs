use aoc_2025::parse_input_file;

fn fold_max(bank: Vec<u32>, len: usize) -> u64 {
    bank.into_iter().fold(vec![0_u64; len], |maxes, i| {
        maxes
            .into_iter()
            .fold((vec![], 0_u64), |(mut acc, carry), next| {
                acc.push(next.max(carry * 10 + i as u64));
                (acc, next)
            })
            .0
    })[len - 1]
}

fn main() {
    let banks = parse_input_file(vec![], |mut acc, next| {
        let bank = next
            .chars()
            .map(|c| c.to_digit(10).unwrap())
            .collect::<Vec<u32>>();
        acc.push(bank);
        acc
    });

    let sum = banks.clone().into_iter().map(|bank| fold_max(bank, 2)).sum::<u64>();
    println!("part 1: {sum}");

    let sum = banks.into_iter().map(|bank| fold_max(bank, 12)).sum::<u64>();
    println!("part 2: {sum}");
}
