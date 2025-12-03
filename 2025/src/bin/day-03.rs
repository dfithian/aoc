use aoc_2025::parse_input_file;

fn main() {
    let banks = parse_input_file(vec![], |mut acc, next| {
        let bank = next
            .chars()
            .map(|c| c.to_digit(10).unwrap())
            .collect::<Vec<u32>>();
        acc.push(bank);
        acc
    });

    let sum = banks.clone().into_iter().fold(0_u64, |sum, bank| {
        let max_jolts = bank
            .into_iter()
            .fold((0_u64, 0_u64), |(double_max, single_max), i| {
                (
                    double_max.max(single_max * 10 + i as u64),
                    single_max.max(i as u64),
                )
            })
            .0;
        sum + max_jolts
    });
    println!("part 1: {sum}");

    let sum = banks.into_iter().fold(0_u64, |sum, bank| {
        let max_jolts = bank
            .into_iter()
            .fold(
                (
                    0_u64, 0_u64, 0_u64, 0_u64, 0_u64, 0_u64, 0_u64, 0_u64, 0_u64, 0_u64, 0_u64,
                    0_u64,
                ),
                |(
                    twelve_max,
                    eleven_max,
                    ten_max,
                    nine_max,
                    eight_max,
                    seven_max,
                    six_max,
                    five_max,
                    four_max,
                    three_max,
                    two_max,
                    one_max,
                ),
                 i| {
                    (
                        twelve_max.max(eleven_max * 10 + i as u64),
                        eleven_max.max(ten_max * 10 + i as u64),
                        ten_max.max(nine_max * 10 + i as u64),
                        nine_max.max(eight_max * 10 + i as u64),
                        eight_max.max(seven_max * 10 + i as u64),
                        seven_max.max(six_max * 10 + i as u64),
                        six_max.max(five_max * 10 + i as u64),
                        five_max.max(four_max * 10 + i as u64),
                        four_max.max(three_max * 10 + i as u64),
                        three_max.max(two_max * 10 + i as u64),
                        two_max.max(one_max * 10 + i as u64),
                        one_max.max(i as u64),
                    )
                },
            )
            .0;
        sum + max_jolts
    });
    println!("part 2: {sum}");
}
