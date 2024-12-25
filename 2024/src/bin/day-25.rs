use aoc_2024::parse_input_file_raw;

enum Kind {
    Lock,
    Key,
}

fn ingest(
    locks: &mut Vec<Vec<u32>>,
    keys: &mut Vec<Vec<u32>>,
    kind: &mut Option<Kind>,
    cur: &mut Vec<Vec<char>>,
    next: String,
) {
    match (&kind, next.as_str()) {
        (Some(k), "") => {
            match k {
                Kind::Lock => locks.push(lock(cur.clone())),
                Kind::Key => keys.push(key(cur.clone())),
            }
            *kind = None;
            *cur = vec![];
        }
        (None, "#####") => *kind = Some(Kind::Lock),
        (None, ".....") => *kind = Some(Kind::Key),
        _ => {
            cur.push(next.chars().collect());
        }
    }
}

// Returns height of a column compared to a character. For locks (`#`), use this value. For keys (`.`), subtract from 5.
fn height(input: &Vec<Vec<char>>, col: usize, char: char) -> u32 {
    (0..5).fold(0, |n, row| {
        if input[row][col] == char {
            n.max(row + 1)
        } else {
            n
        }
    }) as u32
}

fn lock(input: Vec<Vec<char>>) -> Vec<u32> {
    (0..5).map(|col| height(&input, col, '#')).collect()
}

fn key(input: Vec<Vec<char>>) -> Vec<u32> {
    (0..5).map(|col| 5 - height(&input, col, '.')).collect()
}

fn main() {
    let mut locks = vec![];
    let mut keys = vec![];
    let mut kind: Option<Kind> = None;
    let mut cur = vec![];
    parse_input_file_raw((), |(), next| {
        ingest(&mut locks, &mut keys, &mut kind, &mut cur, next)
    });
    ingest(&mut locks, &mut keys, &mut kind, &mut cur, "".to_string());

    let mut part_1 = 0;
    for lock in locks.iter() {
        for key in keys.iter() {
            if (0..5).all(|i| lock[i] + key[i] <= 5) {
                part_1 += 1;
            }
        }
    }
    println!("Part 1: {part_1}");
}
