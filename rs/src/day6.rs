use std::collections::BTreeSet;

fn solve<const N: usize>(data: &str) -> Option<usize> {
    let mut window = ['\0'; N];
    for (i, c) in data.chars().enumerate() {
        window[i % N] = c;
        if i >= N && window.iter().collect::<BTreeSet<_>>().len() == N {
            return Some(i + 1);
        }
    }
    None
}

pub fn part1<'a, I, S>(lines: I) -> Option<usize>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    solve::<4>(lines.into_iter().next()?.as_ref())
}

pub fn part2<'a, I, S>(lines: I) -> Option<usize>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    solve::<14>(lines.into_iter().next()?.as_ref())
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn part1_examples() {
        assert_eq!(Some(7), part1(&["mjqjpqmgbljsphdztnvjfqwrcgsmlb"]));
        assert_eq!(Some(5), part1(&["bvwbjplbgvbhsrlpgdmjqwftvncz"]));
        assert_eq!(Some(6), part1(&["nppdvjthqldpwncqszvftbrmjlhg"]));
        assert_eq!(Some(10), part1(&["nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"]));
        assert_eq!(Some(11), part1(&["zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"]));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(Some(19), part2(&["mjqjpqmgbljsphdztnvjfqwrcgsmlb"]));
        assert_eq!(Some(23), part2(&["bvwbjplbgvbhsrlpgdmjqwftvncz"]));
        assert_eq!(Some(23), part2(&["nppdvjthqldpwncqszvftbrmjlhg"]));
        assert_eq!(Some(29), part2(&["nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"]));
        assert_eq!(Some(26), part2(&["zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"]));
    }
}
