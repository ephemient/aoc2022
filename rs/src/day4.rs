fn parse(line: &str) -> Option<(u32, u32, u32, u32)> {
    let mut iter = line.split([',', '-']);
    Some((
        iter.next()?.parse().ok()?,
        iter.next()?.parse().ok()?,
        iter.next()?.parse().ok()?,
        iter.next()?.parse().ok()?,
    ))
}

pub fn part1<'a, I, S>(lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    lines
        .into_iter()
        .filter_map(|line| parse(line.as_ref()))
        .filter(|(a, b, c, d)| a <= c && b >= d || a >= c && b <= d)
        .count()
}

pub fn part2<'a, I, S>(lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    lines
        .into_iter()
        .filter_map(|line| parse(line.as_ref()))
        .filter(|(a, b, c, d)| a <= d && b >= c)
        .count()
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &[
        "2-4,6-8", "2-3,4-5", "5-7,7-9", "2-8,3-7", "6-6,4-6", "2-6,4-8",
    ];

    #[test]
    fn part1_examples() {
        assert_eq!(2, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(4, part2(EXAMPLE));
    }
}
