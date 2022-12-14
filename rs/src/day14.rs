use super::util::iter_pairs;
use std::cmp::{max, min};
use std::collections::HashSet;

fn parse<'a, I, S>(lines: I) -> HashSet<(isize, isize)>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    lines
        .into_iter()
        .flat_map(|line| {
            iter_pairs(line.as_ref().split(" -> ").filter_map(|pair| {
                let (x, y) = pair.split_once(',')?;
                Some((x.parse::<isize>().ok()?, y.parse::<isize>().ok()?))
            }))
            .flat_map(|((x1, y1), (x2, y2))| {
                (min(x1, x2)..=max(x1, x2))
                    .flat_map(move |x| (min(y1, y2)..=max(y1, y2)).map(move |y| (x, y)))
            })
        })
        .collect()
}

fn fall(blocks: &mut HashSet<(isize, isize)>, max_y: isize) -> Result<(isize, isize), isize> {
    let mut x = 500;
    for y in 0..max_y {
        if !blocks.contains(&(x, y + 1)) {
        } else if !blocks.contains(&(x - 1, y + 1)) {
            x -= 1
        } else if !blocks.contains(&(x + 1, y + 1)) {
            x += 1
        } else {
            return Ok((x, y));
        }
    }
    Err(x)
}

pub fn part1<'a, I, S>(lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut blocks = parse(lines);
    let Some(max_y) = blocks.iter().map(|(_, y)| *y).max() else { return 0 };
    let mut count = 0;
    loop {
        let Ok(point) = fall(&mut blocks, max_y) else { return count };
        blocks.insert(point);
        count += 1;
    }
}

pub fn part2<'a, I, S>(lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut blocks = parse(lines);
    let Some(max_y) = blocks.iter().map(|(_, y)| y + 1).max() else { return 0 };
    let mut count = 0;
    while !blocks.contains(&(500, 0)) {
        let point = fall(&mut blocks, max_y).unwrap_or_else(|x| (x, max_y));
        blocks.insert(point);
        count += 1;
    }
    count
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &[
        "498,4 -> 498,6 -> 496,6",
        "503,4 -> 502,4 -> 502,9 -> 494,9",
    ];

    #[test]
    fn part1_examples() {
        assert_eq!(24, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(93, part2(EXAMPLE));
    }
}
