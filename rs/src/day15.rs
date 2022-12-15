use std::cmp::{max, min};
use std::collections::BTreeSet;
use std::convert::identity;
use std::ops::Range;

struct Intervals<T>(Vec<Range<T>>);
impl<T> Intervals<T> {
    fn new() -> Self {
        Intervals(Vec::new())
    }
}
impl<T: Copy + Ord> Intervals<T> {
    fn add(&mut self, range: Range<T>) {
        let i = self
            .0
            .binary_search_by_key(&range.start, |range| range.end)
            .unwrap_or_else(identity);
        let j = self
            .0
            .binary_search_by_key(&range.end, |range| range.start)
            .unwrap_or_else(identity);
        let range = if i < j {
            min(range.start, self.0[i].start)..max(range.end, self.0[j - 1].end)
        } else {
            range
        };
        self.0.splice(i..j, [range]);
    }
}

fn parse(line: &str) -> Option<((i32, i32), (i32, i32))> {
    let (left, right) = line.split_once(':')?;
    let (left, y0) = left.rsplit_once(", y=")?;
    let (_, x0) = left.rsplit_once("x=")?;
    let (_, right) = right.split_once("x=")?;
    let (x1, y1) = right.split_once(", y=")?;
    Some((
        (x0.parse().ok()?, y0.parse().ok()?),
        (x1.parse().ok()?, y1.parse().ok()?),
    ))
}

pub fn part1<'a, I, S>(y: i32, lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let (acc, set) = lines
        .into_iter()
        .filter_map(|line| parse(line.as_ref()))
        .fold(
            (Intervals::new(), BTreeSet::new()),
            |(mut acc, mut set), ((x0, y0), (x1, y1))| {
                let dx = (x1 - x0).abs() + (y1 - y0).abs() - (y - y0).abs();
                if dx >= 0 {
                    acc.add(x0 - dx..x0 + dx + 1);
                }
                if y1 == y {
                    set.insert(x1);
                }
                (acc, set)
            },
        );
    acc.0
        .into_iter()
        .map(|range| (range.end - range.start) as usize)
        .sum::<usize>()
        - set.len()
}

pub fn part2<'a, I, S>(size: i32, lines: I) -> Option<u64>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let data = lines
        .into_iter()
        .filter_map(|line| parse(line.as_ref()))
        .collect::<Vec<_>>();
    (0..=size)
        .filter_map(|y| {
            data.iter()
                .fold(Intervals::new(), |mut acc, ((x0, y0), (x1, y1))| {
                    let dx = (x1 - x0).abs() + (y1 - y0).abs() - (y - y0).abs();
                    let lo = max(0, x0 - dx);
                    let hi = min(size, x0 + dx);
                    if lo <= hi {
                        acc.add(lo..hi + 1);
                    }
                    acc
                })
                .0
                .into_iter()
                .chain([size + 1..size + 1])
                .scan(0, |acc, range| {
                    let x = Some(*acc).filter(|x| x < &range.start);
                    *acc = range.end;
                    Some(x)
                })
                .find_map(|x| x.map(|x| 4000000 * x as u64 + y as u64))
        })
        .next()
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &[
        "Sensor at x=2, y=18: closest beacon is at x=-2, y=15",
        "Sensor at x=9, y=16: closest beacon is at x=10, y=16",
        "Sensor at x=13, y=2: closest beacon is at x=15, y=3",
        "Sensor at x=12, y=14: closest beacon is at x=10, y=16",
        "Sensor at x=10, y=20: closest beacon is at x=10, y=16",
        "Sensor at x=14, y=17: closest beacon is at x=10, y=16",
        "Sensor at x=8, y=7: closest beacon is at x=2, y=10",
        "Sensor at x=2, y=0: closest beacon is at x=2, y=10",
        "Sensor at x=0, y=11: closest beacon is at x=2, y=10",
        "Sensor at x=20, y=14: closest beacon is at x=25, y=17",
        "Sensor at x=17, y=20: closest beacon is at x=21, y=22",
        "Sensor at x=16, y=7: closest beacon is at x=15, y=3",
        "Sensor at x=14, y=3: closest beacon is at x=15, y=3",
        "Sensor at x=20, y=1: closest beacon is at x=15, y=3",
    ];

    #[test]
    fn part1_examples() {
        assert_eq!(26, part1(10, EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(Some(56000011), part2(20, EXAMPLE));
    }
}
