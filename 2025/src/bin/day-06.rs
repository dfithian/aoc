use aoc_2025::parse_input_file;

fn main() {
    let grid = parse_input_file(vec![], |mut grid, next| {
        grid.push(next.trim().split_whitespace().map(|str| str.to_string()).collect::<Vec<String>>());
        grid
    });

    let mut sum = 0_u64;
    for col in 0..grid[0].len() {
        let mut operands = vec![];
        for row in 0..grid.len() {
            if row < grid.len() - 1 {
                operands.push(grid[row][col].parse::<u64>().unwrap());
            } else {
                sum += match grid[row][col].as_str() {
                    "+" => operands.iter().sum::<u64>(),
                    "*" => operands.iter().product::<u64>(),
                    _ => 0,
                };
            }
        }
    }
    println!("part 1: {sum}");

    let grid = parse_input_file(vec![], |mut grid, next| {
        grid.push(next.chars().collect::<Vec<char>>());
        grid
    });

    let height = grid.len();
    let width = grid.iter().fold(0, |n, next| n.max(next.len()));
    let mut sum = 0_u64;
    let mut operands = vec![];
    for col in 0..width {
        let mut operand = 0;
        for row in 0..height {
            let c = if grid[row].len() >= width - col { grid[row][width - 1 - col] } else { ' ' };
            match c {
                '+' => {
                    sum += operand + operands.iter().sum::<u64>();
                    operands = vec![];
                },
                '*' => {
                    sum += operand * operands.iter().product::<u64>();
                    operands = vec![];
                },
                ' ' => if row == height - 1 && operand > 0 {
                    operands.push(operand);
                },
                '1' => operand = operand * 10 + 1,
                '2' => operand = operand * 10 + 2,
                '3' => operand = operand * 10 + 3,
                '4' => operand = operand * 10 + 4,
                '5' => operand = operand * 10 + 5,
                '6' => operand = operand * 10 + 6,
                '7' => operand = operand * 10 + 7,
                '8' => operand = operand * 10 + 8,
                '9' => operand = operand * 10 + 9,
                _ => (),
            }
        }
    }
    println!("part 2: {sum}");
}
