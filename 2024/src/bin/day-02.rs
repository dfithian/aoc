use aoc_2024::parse_input_file;

fn main() {
    let xss = parse_input_file(vec![], |mut acc, next| {
        acc.push(
            next.split(" ")
                .map(|x| x.parse::<u32>().unwrap())
                .collect::<Vec<_>>(),
        );
        acc
    });

    #[derive(Clone)]
    enum Level {
        None,
        Init(u32),
        Dec(u32),
        Asc(u32),
    }

    impl Level {
        fn check(&mut self, x: u32) -> bool {
            match *self {
                Self::None => {
                    *self = Self::Init(x);
                    return true;
                }
                Self::Init(y) => {
                    *self = if y > x { Self::Dec(x) } else { Self::Asc(x) };
                    return !(x.abs_diff(y) > 3 || x == y);
                }
                Self::Dec(y) => {
                    *self = Level::Dec(x);
                    return !(x.abs_diff(y) > 3 || x >= y);
                }
                Self::Asc(y) => {
                    *self = Level::Asc(x);
                    return !(x.abs_diff(y) > 3 || x <= y);
                }
            }
        }
    }

    fn check_all(xs: &Vec<u32>) -> bool {
        let mut level = Level::None;
        for x in xs {
            if !level.check(*x) {
                return false;
            }
        }
        return true;
    }

    let num_safe = xss.clone().into_iter().filter(check_all).count();
    println!("part 1: {num_safe}");

    let num_safe = xss
        .into_iter()
        .filter(|xs| {
            let mut choices = vec![xs.clone()];
            for i in 0..xs.len() {
                choices.push(
                    xs.iter()
                        .enumerate()
                        .filter(|(pos, _)| *pos != i)
                        .map(|(_, x)| *x)
                        .collect(),
                );
            }
            choices.iter().any(check_all)
        })
        .count();
    println!("part 2: {num_safe}");
}
