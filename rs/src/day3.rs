use itertools::Itertools;
use std::collections::HashSet;

fn prio(item: u8) -> u8 {
    if item & 32 != 0 {
        item & 31
    } else {
        (item & 31) + 26
    }
}

pub fn part1<'a, I, S>(lines: I) -> u32
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    lines
        .into_iter()
        .map(|line| -> u32 {
            let bytes = line.as_ref().as_bytes();
            let (first, second) = bytes.split_at(bytes.len() / 2);
            first
                .iter()
                .copied()
                .collect::<HashSet<_>>()
                .intersection(&second.iter().copied().collect())
                .map(|&item| prio(item) as u32)
                .sum()
        })
        .sum()
}

pub fn part2<'a, I, S>(lines: I) -> u32
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    lines
        .into_iter()
        .chunks(3)
        .into_iter()
        .filter_map(|chunk| {
            chunk
                .fold(None, |acc, line| -> Option<HashSet<u8>> {
                    let items = line.as_ref().as_bytes().iter().copied().collect();
                    Some(
                        acc.map(|acc| acc.intersection(&items).copied().collect())
                            .unwrap_or_else(|| items),
                    )
                })?
                .into_iter()
                .next()
        })
        .map(|item| prio(item) as u32)
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &[
        "vJrwpWtwJgWrhcsFMMfFFhFp",
        "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
        "PmmdzqPrVvPwwTWBwg",
        "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
        "ttgJtRGJQctTZtZT",
        "CrZsJsPPZsGzwwsLwLmpwMDw",
    ];

    #[test]
    fn part1_examples() {
        assert_eq!(157, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(70, part2(EXAMPLE));
    }
}
