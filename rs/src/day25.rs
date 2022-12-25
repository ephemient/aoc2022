pub fn part1<'a, I, S>(lines: I) -> String
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut n = lines
        .into_iter()
        .map(|line| {
            line.as_ref().chars().fold(0, |k, c| {
                5 * k
                    + match c {
                        '=' => -2,
                        '-' => -1,
                        _ => c.to_digit(3).expect("error") as i64,
                    }
            })
        })
        .sum::<i64>();
    let mut res = vec![];
    while n != 0 {
        res.push(['=', '-', '0', '1', '2'][(n + 2).rem_euclid(5) as usize]);
        n = (n + 2).div_euclid(5);
    }
    res.into_iter().rev().collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &[
        "1=-0-2", "12111", "2=0=", "21", "2=01", "111", "20012", "112", "1=-1=", "1-12", "12",
        "1=", "122",
    ];

    #[test]
    fn part1_examples() {
        assert_eq!("2=-1=0", part1(EXAMPLE));
    }
}
