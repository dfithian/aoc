use std::collections::BTreeSet;

use aoc_2025::parse_input_file;

fn adjacency_sum(
    row: usize,
    col: usize,
    max_row: usize,
    max_col: usize,
    grid: &Vec<Vec<u32>>,
) -> u32 {
    let mut n = 0_u32;
    if row != 0 {
        if col != 0 {
            n += grid[row - 1][col - 1];
        }
        n += grid[row - 1][col];
        if col != max_col - 1 {
            n += grid[row - 1][col + 1];
        }
    }
    if col != 0 {
        n += grid[row][col - 1];
    }
    if col != max_col - 1 {
        n += grid[row][col + 1];
    }
    if row != max_row - 1 {
        if col != 0 {
            n += grid[row + 1][col - 1];
        }
        n += grid[row + 1][col];
        if col != max_col - 1 {
            n += grid[row + 1][col + 1];
        }
    }
    n
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

    let max_row = grid.len();
    let max_col = grid[0].len();

    let mut seen = BTreeSet::<(usize, usize)>::new();
    for row in 0..max_row {
        for col in 0..max_col {
            let n = adjacency_sum(row, col, max_row, max_col, &grid);
            if n < 4 && grid[row][col] == 1 {
                seen.insert((row, col));
            }
        }
    }
    let count = seen.len();
    println!("part 1: {count}");

    let mut seen = BTreeSet::<(usize, usize)>::new();
    let mut changed = true;
    while changed {
        changed = false;
        for row in 0..max_row {
            for col in 0..max_col {
                let n = adjacency_sum(row, col, max_row, max_col, &grid);
                if n < 4 && grid[row][col] == 1 {
                    if !seen.contains(&(row, col)) {
                        changed = true;
                    }
                    grid[row][col] = 0;
                    seen.insert((row, col));
                }
            }
        }
    }
    let count = seen.len();
    println!("part 2: {count}");
}
