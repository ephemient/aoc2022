use std::iter;
use std::str::FromStr;

#[derive(Clone, Copy)]
enum Operation {
    Square,
    Mul(u32),
    Add(u32),
}
impl FromStr for Operation {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "new = old * old" {
            Ok(Operation::Square)
        } else if let Some(x) = s.strip_prefix("new = old * ") {
            x.parse::<u32>().map(Operation::Mul).map_err(|_| ())
        } else if let Some(x) = s.strip_prefix("new = old + ") {
            x.parse::<u32>().map(Operation::Add).map_err(|_| ())
        } else {
            Err(())
        }
    }
}
impl Operation {
    fn apply(&self, x: u64) -> u64 {
        match self {
            Self::Square => x * x,
            Self::Mul(y) => x * *y as u64,
            Self::Add(y) => x + *y as u64,
        }
    }
}

struct Monkey {
    items: Vec<u64>,
    operation: Operation,
    test: u32,
    if_true: usize,
    if_false: usize,
}

fn parse<'a, I, S>(lines: I) -> Vec<Monkey>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut iter = lines.into_iter().map(|line| line.as_ref());
    iter::from_fn(|| {
        iter.find(|line| !line.is_empty())?;
        Some(Monkey {
            items: iter
                .next()?
                .rsplit_once(':')?
                .1
                .split(',')
                .map(|word| word.trim().parse().ok())
                .collect::<Option<_>>()?,
            operation: iter.next()?.rsplit_once(": ")?.1.parse().ok()?,
            test: iter
                .next()?
                .split_ascii_whitespace()
                .rev()
                .next()?
                .parse()
                .ok()?,
            if_true: iter
                .next()?
                .split_ascii_whitespace()
                .rev()
                .next()?
                .parse()
                .ok()?,
            if_false: iter
                .next()?
                .split_ascii_whitespace()
                .rev()
                .next()?
                .parse()
                .ok()?,
        })
    })
    .collect()
}

fn solve<F: FnMut(u64) -> u64>(monkeys: &mut [Monkey], n: usize, mut f: F) -> u64 {
    let mut counts = vec![0; monkeys.len()];
    for _ in 0..n {
        for i in 0..monkeys.len() {
            let monkey = &mut monkeys[i];
            let items = monkey.items.drain(..).collect::<Vec<_>>();
            counts[i] += items.len();
            let Monkey {
                operation,
                test,
                if_true,
                if_false,
                ..
            } = *monkey;
            for item in items {
                let item = f(operation.apply(item));
                let j = if item % test as u64 == 0 {
                    if_true
                } else {
                    if_false
                };
                monkeys[j].items.push(item);
            }
        }
    }
    counts.sort();
    counts
        .iter()
        .rev()
        .take(2)
        .fold(1, |acc, x| acc * (*x as u64))
}

pub fn part1<'a, I, S>(lines: I) -> u64
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    solve(&mut parse(lines), 20, |x| x / 3)
}

pub fn part2<'a, I, S>(lines: I) -> u64
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut monkeys = parse(lines);
    let base = monkeys
        .iter()
        .fold(1, |acc, monkey| acc * monkey.test as u64);
    solve(&mut monkeys, 10000, |x| x % base)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &[
        "Monkey 0:",
        "  Starting items: 79, 98",
        "  Operation: new = old * 19",
        "  Test: divisible by 23",
        "    If true: throw to monkey 2",
        "    If false: throw to monkey 3",
        "",
        "Monkey 1:",
        "  Starting items: 54, 65, 75, 74",
        "  Operation: new = old + 6",
        "  Test: divisible by 19",
        "    If true: throw to monkey 2",
        "    If false: throw to monkey 0",
        "",
        "Monkey 2:",
        "  Starting items: 79, 60, 97",
        "  Operation: new = old * old",
        "  Test: divisible by 13",
        "    If true: throw to monkey 1",
        "    If false: throw to monkey 3",
        "",
        "Monkey 3:",
        "  Starting items: 74",
        "  Operation: new = old + 3",
        "  Test: divisible by 17",
        "    If true: throw to monkey 0",
        "    If false: throw to monkey 1",
    ];

    #[test]
    fn part1_examples() {
        assert_eq!(10605, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(2713310158, part2(EXAMPLE));
    }
}
