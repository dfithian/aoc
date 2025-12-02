use std::collections::BTreeSet;

use aoc_2025::parse_input_file;

fn get_digits(n: u64) -> Vec<u64> {
    let mut cur = n;
    let mut digits = vec![];
    while cur > 0 {
        digits.push(cur % 10);
        cur /= 10;
    }
    digits.reverse();
    digits
}

fn slice_to_u64(arr: &[u64]) -> u64 {
    arr.into_iter().fold(0_u64, |acc, next| acc * 10 + next)
}

fn main() {
    let ranges = parse_input_file(vec![], |_, next| {
        next.split(",").into_iter().map(|range| {
            let ids = range.split("-").collect::<Vec<&str>>();
            let start = ids[0].parse::<u64>().unwrap();
            let end = ids[1].parse::<u64>().unwrap();
            (start, get_digits(start), end, get_digits(end))
        }).collect::<Vec<(u64, Vec<u64>, u64, Vec<u64>)>>()
    });

    let sum = ranges.clone().into_iter().fold(0_u64, |mut sum, (start, start_digits, end, end_digits)| {
        let mut half_num_digits = (start_digits.len() / 2) as u32;
        let half_num_end_digits = ((end_digits.len() + 1) / 2) as u32;
        let mut prefix = slice_to_u64(&start_digits[0..(half_num_digits as usize)]);
        let prefix_end = slice_to_u64(&end_digits[0..(half_num_end_digits as usize)]);
        let mut biggest_prefix = slice_to_u64(&vec![9; half_num_digits as usize]);
        while prefix <= prefix_end {
            let n = prefix * 10_u64.pow(half_num_digits) + prefix;
            if start <= n && n <= end {
                sum += n;
            }
            prefix += 1;
            if prefix > biggest_prefix {
                half_num_digits += 1;
                biggest_prefix = biggest_prefix * 10 + 9;
            }
        }

        sum
    });
    println!("part 1: {sum}");

    let sum = ranges.clone().into_iter().fold(0_u64, |mut sum, (start, _start_digits, end, end_digits)| {
        let half_num_end_digits = ((end_digits.len() + 1) / 2) as u32;
        let prefix_end = slice_to_u64(&end_digits[0..(half_num_end_digits as usize)]);
        let mut seen = BTreeSet::<u64>::new();
        let mut num_digits = 1_u32;
        let mut prefix = 1;
        let mut biggest_prefix = slice_to_u64(&vec![9; num_digits as usize]);
        while num_digits <= half_num_end_digits {
            while prefix <= prefix_end {
                let mut n = prefix;
                while n < start {
                    n = n * 10_u64.pow(num_digits) + prefix;
                }
                if start <= n && n <= end && !seen.contains(&n) {
                    seen.insert(n);
                    sum += n;
                }
                prefix += 1;
                if prefix > biggest_prefix {
                    num_digits += 1;
                    biggest_prefix = biggest_prefix * 10 + 9;
                }
            }
            num_digits += 1;
        }
        sum
    });
    println!("part 2: {sum}");
}
