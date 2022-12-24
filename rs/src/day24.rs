use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashSet};
use std::ops::Range;

fn add_mod_range(a: usize, b: usize, range: Range<usize>) -> usize {
    (a - range.start + b) % range.len() + range.start
}

fn sub_mod_range(a: usize, b: usize, range: Range<usize>) -> usize {
    (a - range.start + range.len() - b % range.len()) % range.len() + range.start
}

fn search(
    lines: &[&[u8]],
    start: (usize, usize),
    end: (usize, usize),
    time: usize,
) -> Option<usize> {
    let mut seen: HashSet<_> = [(start, time)].into();
    let mut queue: BinaryHeap<_> = [(Reverse(0), (start, time))].into();
    while let Some((_, (pos @ (x, y), time))) = queue.pop() {
        if pos == end {
            return Some(time);
        }
        for (dx, dy) in [(-1, 0), (0, -1), (0, 0), (0, 1), (1, 0)] {
            let Some(x) = x.checked_add_signed(dx) else { continue };
            let Some(y) = y.checked_add_signed(dy) else { continue };
            if y >= lines.len() || x == 0 {
                continue;
            }
            let line = lines[y];
            if x >= line.len() - 1 {
                continue;
            }
            if y == 0 || y == lines.len() - 1 {
                if lines[y][x] != b'.' {
                    continue;
                }
            } else if line[add_mod_range(x, time + 1, 1..line.len() - 1)] == b'<'
                || line[sub_mod_range(x, time + 1, 1..line.len() - 1)] == b'>'
                || lines[add_mod_range(y, time + 1, 1..lines.len() - 1)][x] == b'^'
                || lines[sub_mod_range(y, time + 1, 1..lines.len() - 1)][x] == b'v'
            {
                continue;
            }
            if seen.insert(((x, y), time + 1)) {
                queue.push((
                    Reverse(time + x.abs_diff(end.0) + y.abs_diff(end.1)),
                    ((x, y), time + 1),
                ));
            }
        }
    }
    None
}

pub fn part1<'a, I, S>(lines: I) -> Option<usize>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let lines = lines
        .into_iter()
        .map(|line| line.as_ref().as_bytes())
        .collect::<Vec<_>>();
    search(
        &lines,
        (1, 0),
        (lines.last()?.len() - 2, lines.len() - 1),
        0,
    )
}

pub fn part2<'a, I, S>(lines: I) -> Option<usize>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let lines = lines
        .into_iter()
        .map(|line| line.as_ref().as_bytes())
        .collect::<Vec<_>>();
    let start = (1, 0);
    let end = (lines.last()?.len() - 2, lines.len() - 1);
    search(
        &lines,
        start,
        end,
        search(&lines, end, start, search(&lines, start, end, 0)?)?,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &[
        "#.######", "#>>.<^<#", "#.<..<<#", "#>v.><>#", "#<^v^^>#", "######.#",
    ];

    #[test]
    fn part1_examples() {
        assert_eq!(Some(18), part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(Some(54), part2(EXAMPLE));
    }
}
