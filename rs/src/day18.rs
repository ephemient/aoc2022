use std::cmp::{max, min};
use std::collections::BTreeSet;

fn parse_point(s: &str) -> Option<(i32, i32, i32)> {
    let mut iter = s.splitn(3, ',');
    Some((
        iter.next()?.parse().ok()?,
        iter.next()?.parse().ok()?,
        iter.next()?.parse().ok()?,
    ))
}

fn neighbors((x, y, z): (i32, i32, i32)) -> [(i32, i32, i32); 6] {
    [
        (x - 1, y, z),
        (x + 1, y, z),
        (x, y - 1, z),
        (x, y + 1, z),
        (x, y, z - 1),
        (x, y, z + 1),
    ]
}

pub fn part1<'a, I, S>(lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let points = lines
        .into_iter()
        .filter_map(|line| parse_point(line.as_ref()))
        .collect::<BTreeSet<_>>();
    points
        .iter()
        .copied()
        .flat_map(neighbors)
        .filter(|point| !points.contains(point))
        .count()
}

pub fn part2<'a, I, S>(lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let (mut min_x, mut max_x, mut min_y, mut max_y, mut min_z, mut max_z) =
        (i32::MAX, i32::MIN, i32::MAX, i32::MIN, i32::MAX, i32::MIN);
    let points = lines
        .into_iter()
        .filter_map(|line| parse_point(line.as_ref()))
        .inspect(|&(x, y, z)| {
            min_x = min(min_x, x);
            max_x = max(max_x, x);
            min_y = min(min_y, y);
            max_y = max(max_y, y);
            min_z = min(min_z, z);
            max_z = max(max_z, z);
        })
        .collect::<BTreeSet<_>>();
    let mut queue = vec![(min_x - 1, min_y - 1, min_z - 1)];
    let mut outside = queue.iter().copied().collect::<BTreeSet<_>>();
    while let Some(point) = queue.pop() {
        for (x, y, z) in neighbors(point) {
            if (min_x - 1..=max_x + 1).contains(&x)
                && (min_y - 1..=max_y + 1).contains(&y)
                && (min_z - 1..=max_z + 1).contains(&z)
                && !points.contains(&(x, y, z))
                && outside.insert((x, y, z))
            {
                queue.push((x, y, z));
            }
        }
    }
    points
        .iter()
        .copied()
        .flat_map(neighbors)
        .filter(|point| outside.contains(point))
        .count()
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &[
        "2,2,2", "1,2,2", "3,2,2", "2,1,2", "2,3,2", "2,2,1", "2,2,3", "2,2,4", "2,2,6", "1,2,5",
        "3,2,5", "2,1,5", "2,3,5",
    ];

    #[test]
    fn part1_examples() {
        assert_eq!(64, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(58, part2(EXAMPLE));
    }
}
