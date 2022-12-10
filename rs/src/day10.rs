use itertools::Itertools;
use std::str::FromStr;

enum Instruction {
    Noop,
    AddX(i32),
}
impl FromStr for Instruction {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "noop" {
            return Ok(Instruction::Noop);
        }
        let Some(num) = s.strip_prefix("addx ") else {
            return Err(())
        };
        let Ok(num) = num.parse() else {
            return Err(())
        };
        Ok(Instruction::AddX(num))
    }
}

pub fn part1<'a, I, S>(lines: I) -> i32
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut last_x = 1;
    let mut iter = lines
        .into_iter()
        .scan((0, 1), |(cycle, x), line| {
            match line.as_ref().parse::<Instruction>().ok()? {
                Instruction::Noop => *cycle += 1,
                Instruction::AddX(dx) => {
                    *cycle += 2;
                    *x += dx;
                }
            };
            Some((*cycle, *x))
        })
        .peekable();
    [20, 60, 100, 140, 180, 220]
        .iter()
        .map(|tap| {
            while let Some((_, x)) = iter.peek().filter(|(cycle, _)| cycle < tap) {
                last_x = *x;
                iter.next();
            }
            tap * last_x
        })
        .sum()
}

pub fn part2<'a, I, S>(lines: I) -> String
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut last_x = 1;
    let mut iter = lines
        .into_iter()
        .scan((0, 1), |(cycle, x), line| {
            match line.as_ref().parse::<Instruction>().ok()? {
                Instruction::Noop => *cycle += 1,
                Instruction::AddX(dx) => {
                    *cycle += 2;
                    *x += dx;
                }
            };
            Some((*cycle, *x))
        })
        .peekable();
    (0..240)
        .step_by(40)
        .map(|row| {
            (0..40)
                .map(|col| {
                    while let Some((_, x)) = iter.peek().filter(|(cycle, _)| cycle <= &(row + col))
                    {
                        last_x = *x;
                        iter.next();
                    }
                    if (-1..=1).contains(&(last_x - col)) {
                        '\u{2593}'
                    } else {
                        '\u{2591}'
                    }
                })
                .collect::<String>()
        })
        .join("\n")
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &[
        "addx 15", "addx -11", "addx 6", "addx -3", "addx 5", "addx -1", "addx -8", "addx 13",
        "addx 4", "noop", "addx -1", "addx 5", "addx -1", "addx 5", "addx -1", "addx 5", "addx -1",
        "addx 5", "addx -1", "addx -35", "addx 1", "addx 24", "addx -19", "addx 1", "addx 16",
        "addx -11", "noop", "noop", "addx 21", "addx -15", "noop", "noop", "addx -3", "addx 9",
        "addx 1", "addx -3", "addx 8", "addx 1", "addx 5", "noop", "noop", "noop", "noop", "noop",
        "addx -36", "noop", "addx 1", "addx 7", "noop", "noop", "noop", "addx 2", "addx 6", "noop",
        "noop", "noop", "noop", "noop", "addx 1", "noop", "noop", "addx 7", "addx 1", "noop",
        "addx -13", "addx 13", "addx 7", "noop", "addx 1", "addx -33", "noop", "noop", "noop",
        "addx 2", "noop", "noop", "noop", "addx 8", "noop", "addx -1", "addx 2", "addx 1", "noop",
        "addx 17", "addx -9", "addx 1", "addx 1", "addx -3", "addx 11", "noop", "noop", "addx 1",
        "noop", "addx 1", "noop", "noop", "addx -13", "addx -19", "addx 1", "addx 3", "addx 26",
        "addx -30", "addx 12", "addx -1", "addx 3", "addx 1", "noop", "noop", "noop", "addx -9",
        "addx 18", "addx 1", "addx 2", "noop", "noop", "addx 9", "noop", "noop", "noop", "addx -1",
        "addx 2", "addx -37", "addx 1", "addx 3", "noop", "addx 15", "addx -21", "addx 22",
        "addx -6", "addx 1", "noop", "addx 2", "addx 1", "noop", "addx -10", "noop", "noop",
        "addx 20", "addx 1", "addx 2", "addx 2", "addx -6", "addx -11", "noop", "noop", "noop",
    ];

    #[test]
    fn part1_examples() {
        assert_eq!(13140, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(
            [
                "▓▓░░▓▓░░▓▓░░▓▓░░▓▓░░▓▓░░▓▓░░▓▓░░▓▓░░▓▓░░",
                "▓▓▓░░░▓▓▓░░░▓▓▓░░░▓▓▓░░░▓▓▓░░░▓▓▓░░░▓▓▓░",
                "▓▓▓▓░░░░▓▓▓▓░░░░▓▓▓▓░░░░▓▓▓▓░░░░▓▓▓▓░░░░",
                "▓▓▓▓▓░░░░░▓▓▓▓▓░░░░░▓▓▓▓▓░░░░░▓▓▓▓▓░░░░░",
                "▓▓▓▓▓▓░░░░░░▓▓▓▓▓▓░░░░░░▓▓▓▓▓▓░░░░░░▓▓▓▓",
                "▓▓▓▓▓▓▓░░░░░░░▓▓▓▓▓▓▓░░░░░░░▓▓▓▓▓▓▓░░░░░"
            ]
            .join("\n"),
            part2(EXAMPLE)
        );
    }
}
