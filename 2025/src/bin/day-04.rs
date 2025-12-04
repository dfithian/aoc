use std::collections::BTreeSet;

use aoc_2025::parse_input_file;

fn adjacency_sum(row: i32, col: i32, max_row: i32, max_col: i32, grid: &Vec<Vec<u32>>) -> u32 {
    vec![
        (row - 1, col - 1),
        (row - 1, col),
        (row - 1, col + 1),
        (row, col - 1),
        (row, col + 1),
        (row + 1, col - 1),
        (row + 1, col),
        (row + 1, col + 1),
    ]
    .into_iter()
    .fold(0_u32, |n, (i, j)| {
        if 0 <= i && i < max_row && 0 <= j && j < max_col {
            n + grid[i as usize][j as usize]
        } else {
            n
        }
    })
}

fn main() {
    let mut grid = parse_input_file(vec![], |mut acc, line| {
        let rolls = line
            .chars()
            .map(|c| match c {
                '@' => 1_u32,
                _ => 0_u32,
            })
            .collect::<Vec<u32>>();
        acc.push(rolls);
        acc
    });

    let max_row = grid.len() as i32;
    let max_col = grid[0].len() as i32;

    let mut seen = BTreeSet::<(i32, i32)>::new();
    for row in 0..max_row {
        for col in 0..max_col {
            if grid[row as usize][col as usize] == 1 {
                if adjacency_sum(row, col, max_row, max_col, &grid) < 4 {
                    seen.insert((row, col));
                }
            }
        }
    }
    let count = seen.len();
    println!("part 1: {count}");

    let mut seen = BTreeSet::<(i32, i32)>::new();
    let mut changed = true;
    while changed {
        changed = false;
        for row in 0..max_row {
            for col in 0..max_col {
                if grid[row as usize][col as usize] == 1 {
                    if adjacency_sum(row, col, max_row, max_col, &grid) < 4 {
                        if !seen.contains(&(row, col)) {
                            changed = true;
                            grid[row as usize][col as usize] = 0;
                            seen.insert((row, col));
                        }
                    }
                }
            }
        }
    }
    let count = seen.len();
    println!("part 2: {count}");
}
