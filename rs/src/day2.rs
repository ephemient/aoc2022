use std::vec::Vec;

fn parse<'a, I, S>(lines: I) -> Vec<(u8, u8)>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    lines
        .into_iter()
        .filter_map(|line| {
            let mut iter = line.as_ref().split_ascii_whitespace();
            Some((
                iter.next()?.as_bytes().first()? - 64,
                iter.next()?.as_bytes().first()? - 87,
            ))
        })
        .collect()
}

fn score(left: u8, right: u8) -> u8 {
    (4 + right - left) % 3 * 3 + right
}

pub fn part1<'a, I, S>(lines: I) -> u32
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    parse(lines)
        .into_iter()
        .map(|(left, right)| score(left, right) as u32)
        .sum()
}

pub fn part2<'a, I, S>(lines: I) -> u32
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    parse(lines)
        .into_iter()
        .map(|(left, right)| score(left, 1 + (left + right) % 3) as u32)
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &["A Y", "B X", "C Z"];

    #[test]
    fn part1_examples() {
        assert_eq!(15, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(12, part2(EXAMPLE));
    }
}
