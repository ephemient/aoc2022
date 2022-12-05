use std::collections::BTreeMap;

fn solve<'a, const R: bool, I, S>(lines: I) -> String
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut iter = lines.into_iter();
    let mut columns: Vec<String> = vec![];
    for line in iter.by_ref() {
        let line = line.as_ref();
        if line.is_empty() {
            break;
        }
        for (i, c) in line.chars().enumerate() {
            match columns.get_mut(i) {
                Some(column) => column.push(c),
                _ => columns.push(c.to_string()),
            }
        }
    }
    let mut stacks: BTreeMap<char, String> = columns
        .into_iter()
        .filter_map(|mut column| {
            let key = column.pop().filter(char::is_ascii_digit)?;
            Some((key, column.trim_start().to_string()))
        })
        .collect();
    iter.filter_map(|line| {
        let mut iter = line.as_ref().split_ascii_whitespace();
        let num: usize = iter.nth(1)?.parse().ok()?;
        let x = iter.nth(1)?.chars().next()?;
        let y = iter.nth(1)?.chars().next()?;
        let source = stacks.get_mut(&x)?;
        let head = if R {
            source.drain(..num).rev().collect()
        } else {
            source.drain(..num).as_str().to_string()
        };
        let dest = stacks.get_mut(&y)?;
        *dest = head + &dest;
        Some(())
    })
    .for_each(drop);
    stacks
        .into_iter()
        .filter_map(|(_, stack)| stack.chars().next())
        .collect()
}

pub fn part1<'a, I, S>(lines: I) -> String
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    solve::<true, _, _>(lines)
}

pub fn part2<'a, I, S>(lines: I) -> String
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    solve::<false, _, _>(lines)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &[
        "    [D]    ",
        "[N] [C]    ",
        "[Z] [M] [P]",
        " 1   2   3 ",
        "",
        "move 1 from 2 to 1",
        "move 3 from 1 to 3",
        "move 2 from 2 to 1",
        "move 1 from 1 to 2",
    ];

    #[test]
    fn part1_examples() {
        assert_eq!("CMZ", part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!("MCD", part2(EXAMPLE));
    }
}
