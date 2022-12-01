use std::cmp::Reverse;
use std::vec::Vec;

fn parse<'a, I, S>(lines: I) -> Vec<u32>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut sums = Vec::new();
    let mut sum = 0;
    for line in lines {
        match line.as_ref().parse::<u32>() {
            Ok(num) => sum += num,
            Err(_) => {
                sums.push(sum);
                sum = 0;
            }
        }
    }
    sums.push(sum);
    sums
}

pub fn part1<'a, I, S>(lines: I) -> u32
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let nums = parse(lines);
    nums.into_iter().max().unwrap_or(0)
}

pub fn part2<'a, I, S>(lines: I) -> u32
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut nums = parse(lines);
    let (maxs, &mut max, _) = nums[..].select_nth_unstable_by_key(2, |&num| Reverse(num));
    maxs.iter_mut().fold(max, |x, &mut y| x + y)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &[
        "1000", "2000", "3000", "", "4000", "", "5000", "6000", "", "7000", "8000", "9000", "",
        "10000",
    ];

    #[test]
    fn part1_examples() {
        assert_eq!(24000, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(45000, part2(EXAMPLE));
    }
}
