use itertools::Itertools;

fn items(input: &[u8]) -> u64 {
    input.iter().fold(0, |acc, byte| {
        acc | (1
            << (if byte & 32 != 0 {
                byte & 31
            } else {
                (byte & 31) + 26
            }))
    })
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
            let bits = items(first) & items(second);
            (0..u64::BITS).filter(|b| bits & (1 << b) != 0).sum()
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
                .map(|line| items(line.as_ref().as_bytes()))
                .reduce(|x, y| x & y)
        })
        .map(u64::trailing_zeros)
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
