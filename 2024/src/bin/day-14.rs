use aoc_2024::parse_input_file;

#[derive(Debug)]
struct Robot {
    position: (i64, i64),
    velocity: (i64, i64),
}

impl Robot {
    fn next(&mut self, bounds: (i64, i64)) {
        self.position.0 = (self.position.0 + self.velocity.0 + bounds.0) % bounds.0;
        self.position.1 = (self.position.1 + self.velocity.1 + bounds.1) % bounds.1;
    }
}

fn visualize(bounds: (i64, i64), robots: &Vec<Robot>) -> String {
    let mut s = format!("");
    for row in 0..bounds.0 {
        for col in 0..bounds.1 {
            let c = robots.iter().filter(|r| r.position == (row, col)).count();
            if c == 0 {
                s = format!("{s}.");
            } else {
                s = format!("{s}{c}");
            }
        }
        s = format!("{s}\n");
    }
    s
}

fn main() {
    let mut robots = parse_input_file(vec![], |mut acc, line| {
        let mut input = line.split(" ");
        let mut p = input
            .next()
            .unwrap()
            .strip_prefix("p=")
            .unwrap()
            .split(",")
            .into_iter();
        let x = p.next().unwrap().parse::<i64>().unwrap();
        let y = p.next().unwrap().parse::<i64>().unwrap();
        let mut v = input
            .next()
            .unwrap()
            .strip_prefix("v=")
            .unwrap()
            .split(",")
            .into_iter();
        let dx = v.next().unwrap().parse::<i64>().unwrap();
        let dy = v.next().unwrap().parse::<i64>().unwrap();
        acc.push(Robot {
            position: (x, y),
            velocity: (dx, dy),
        });
        acc
    });

    // let bounds = (11, 7);
    let bounds = (101, 103);
    let mid_x = bounds.0 / 2;
    let mid_y = bounds.1 / 2;

    let mut n = 0;
    while n < 100 {
        let mut i = 0;
        while i < robots.len() {
            robots[i].next(bounds);
            i += 1;
        }
        n += 1;
    }
    let (x, y, z, w) = robots.iter().fold((0, 0, 0, 0), |mut acc, next| {
        if next.position.0 < mid_x {
            if next.position.1 < mid_y {
                acc.0 += 1;
            } else if next.position.1 > mid_y {
                acc.2 += 1;
            }
        } else if next.position.0 > mid_x {
            if next.position.1 < mid_y {
                acc.1 += 1;
            } else if next.position.1 > mid_y {
                acc.3 += 1;
            }
        }
        acc
    });
    let part_1 = x * y * z * w;
    println!("Part 1: {part_1}");

    loop {
        let mut i = 0;
        while i < robots.len() {
            robots[i].next(bounds);
            i += 1;
        }
        println!("{}", visualize(bounds, &robots));
        println!("{n}");
        n += 1;
    }
}
