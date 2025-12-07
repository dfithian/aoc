use aoc_2025::parse_input_file_char_grid;

fn main() {
    let grid = parse_input_file_char_grid();

    let mut splits = 0;
    let mut tree = vec![vec![false; grid[0].len()]; grid.len()];
    for row in 0..grid.len() {
        for col in 0..grid[0].len() {
            match grid[row][col] {
                'S' => tree[row][col] = true,
                '.' => if row > 0 {
                    tree[row][col] = tree[row][col] || tree[row - 1][col];
                },
                '^' => if tree[row - 1][col] {
                    splits += 1;
                    if col > 0 {
                        tree[row][col - 1] = true;
                    }
                    if col < grid[0].len() - 1 {
                        tree[row][col + 1] = true;
                    }
                },
                _ => (),
            }
        }
    }
    println!("part 1: {splits}");

    let mut tree = vec![vec![0_u64; grid[0].len()]; grid.len()];
    for row in 0..grid.len() {
        for col in 0..grid[0].len() {
            match grid[row][col] {
                'S' => tree[row][col] = 1,
                '.' => if row > 0 {
                    tree[row][col] += tree[row - 1][col];
                },
                '^' => {
                    if col > 0 {
                        tree[row][col - 1] += tree[row - 1][col];
                    }
                    if col < grid[0].len() - 1 {
                        tree[row][col + 1] += tree[row - 1][col];
                    }
                },
                _ => (),
            }
        }
    }
    let splits = tree[grid.len() - 1].iter().sum::<u64>();
    println!("part 2: {splits}");
}
