use aoc_2024::parse_input_file;

fn main() {
    let disk_map = parse_input_file(vec![], |mut ns, next| {
        ns.extend(next.chars().map(|c| format!("{c}").parse::<u64>().unwrap()));
        ns
    });

    let mut i = 0 as usize;
    let mut j = disk_map.len() - 1;
    let mut cur_used_x = 0;
    let mut cur_used_y = 0;
    let mut pos = 0;
    let mut checksum = 0;
    while i <= j {
        let x = disk_map[i];
        let y = disk_map[j];
        if i == j {
            // shore up remainder
            cur_used_x += cur_used_y;
            while cur_used_x < x {
                checksum += pos * (i / 2) as u64;
                pos += 1;
                cur_used_x += 1;
            }
        } else if i % 2 == 0 {
            // use up x
            while cur_used_x < x {
                checksum += pos * (i / 2) as u64;
                pos += 1;
                cur_used_x += 1;
            }
        } else {
            // use up y in the free space between x
            while cur_used_x < x && cur_used_y < y {
                checksum += pos * (j / 2) as u64;
                pos += 1;
                cur_used_x += 1;
                cur_used_y += 1;
            }
        }
        if cur_used_x == x {
            cur_used_x = 0;
            i += 1;
        }
        if cur_used_y == y {
            cur_used_y = 0;
            j -= 2;
        }
    }
    println!("part 1: {checksum}");
}
