use std::collections::VecDeque;

use aoc_2025::parse_input_file_split;

const PRINT: bool = false;
fn print_cover(
    added: Option<(u64, u64, usize)>,
    removed: &Option<Vec<(u64, u64)>>,
    cover: &VecDeque<(u64, u64)>,
) {
    if PRINT {
        println!("   -------------------------------");
        for i in 0..cover.len() {
            if let Some((from, to, row)) = added {
                if i == row {
                    if let Some(removed) = removed {
                        for (removed_from, removed_to) in removed.iter() {
                            println!(" - {:015} {:015}", removed_from, removed_to);
                        }
                    }
                    println!(" + {:015} {:015}", from, to);
                    println!("=> {:015} {:015}", cover[i].0, cover[i].1);
                } else if (row as i32 - 1) <= (i as i32) && (i as i32) <= (row as i32 + 1) {
                    println!("   {:015} {:015}", cover[i].0, cover[i].1);
                }
            } else {
                println!("   {:015} {:015}", cover[i].0, cover[i].1);
            }
        }
    }
}

fn overlaps(from: u64, to: u64, lo: u64, hi: u64) -> bool {
    (from <= lo && hi <= to)
        || (lo <= from && to <= hi)
        || (lo <= from && from <= hi)
        || (lo <= to && to <= hi)
        || hi == from - 1
        || to + 1 == lo
}

fn main() {
    let (ranges, available) = parse_input_file_split(
        (vec![], vec![]),
        |(mut acc_ranges, acc_available), next| {
            let mut range = next.split("-");
            let from = range.next().unwrap().parse::<u64>().unwrap();
            let to = range.next().unwrap().parse::<u64>().unwrap();
            acc_ranges.push((from, to));
            (acc_ranges, acc_available)
        },
        |(acc_ranges, mut acc_available), next| {
            let available = next.parse::<u64>().unwrap();
            acc_available.push(available);
            (acc_ranges, acc_available)
        },
    );

    let total = available.into_iter().filter(|x| ranges.iter().find(|(from, to)| from <= x && x <= to).is_some()).collect::<Vec<u64>>().len();
    println!("part 1: {total}");

    let mut cover = VecDeque::<(u64, u64)>::new();
    for (mut from, mut to) in ranges.iter() {
        let mut drained: Option<Vec<(u64, u64)>> = None;
        let mut i = 0;
        while i < cover.len() {
            if overlaps(from, to, cover[i].0, cover[i].1) {
                let mut j = i;
                while j < cover.len() && overlaps(from, to, cover[j].0, cover[j].1) {
                    from = from.min(cover[j].0);
                    to = to.max(cover[j].1);
                    j += 1;
                }
                drained = Some(cover.drain(i..j).collect::<Vec<(u64, u64)>>());
                break;
            }
            if cover[i].0 > from {
                break;
            }
            i += 1;
        }
        if cover.len() > 0 && i < cover.len() {
            cover.insert(i, (from, to));
        } else if cover.len() > 0 {
            cover.push_back((from, to));
        } else {
            cover.push_front((from, to));
        }
        print_cover(Some((from, to, i)), &drained, &cover);
    }
    print_cover(None, &None, &cover);
    let total = cover.iter().map(|(from, to)| to - from + 1).sum::<u64>();
    println!("part 2: {total}");
}
