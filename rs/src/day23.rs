use self::Direction::*;
use std::array;
use std::cmp::{max, min};
use std::collections::{BTreeMap, BTreeSet};
use std::iter::Cycle;

#[derive(Clone, Copy)]
enum Direction {
    N,
    S,
    W,
    E,
}

fn directions() -> Cycle<array::IntoIter<[Direction; 4], 4>> {
    [[N, S, W, E], [S, W, E, N], [W, E, N, S], [E, N, S, W]]
        .into_iter()
        .cycle()
}

fn sides(direction: Direction, x: isize, y: isize) -> [(isize, isize); 3] {
    match direction {
        N => [x - 1, x, x + 1].map(|x| (x, y - 1)),
        S => [x - 1, x, x + 1].map(|x| (x, y + 1)),
        W => [y - 1, y, y + 1].map(|y| (x - 1, y)),
        E => [y - 1, y, y + 1].map(|y| (x + 1, y)),
    }
}

fn r#move(direction: Direction, x: isize, y: isize) -> (isize, isize) {
    match direction {
        N => (x, y - 1),
        S => (x, y + 1),
        W => (x - 1, y),
        E => (x + 1, y),
    }
}

fn neighbors(x: isize, y: isize) -> [(isize, isize); 8] {
    [
        (x - 1, y - 1),
        (x - 1, y),
        (x - 1, y + 1),
        (x, y - 1),
        (x, y + 1),
        (x + 1, y - 1),
        (x + 1, y),
        (x + 1, y + 1),
    ]
}

fn parse<'a, I, S>(lines: I) -> BTreeSet<(isize, isize)>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    lines
        .into_iter()
        .enumerate()
        .flat_map(|(y, line)| {
            line.as_ref()
                .chars()
                .enumerate()
                .filter(|(_, c)| c == &'#')
                .map(move |(x, _)| (x as isize, y as isize))
        })
        .collect()
}

fn step(state: &BTreeSet<(isize, isize)>, directions: &[Direction]) -> BTreeSet<(isize, isize)> {
    let mut proposals = BTreeMap::new();
    for &(x, y) in state {
        if !neighbors(x, y).iter().any(|point| state.contains(point)) {
            continue;
        }
        if let Some(&direction) = directions.iter().find(|&&direction| {
            !sides(direction, x, y)
                .iter()
                .any(|point| state.contains(point))
        }) {
            proposals
                .entry(r#move(direction, x, y))
                .and_modify(|e: &mut Vec<_>| e.push((x, y)))
                .or_insert_with(|| vec![(x, y)]);
        }
    }
    proposals.retain(|_, old| old.len() == 1);
    let mut state = state.clone();
    for (new, old) in proposals {
        state.remove(&old[0]);
        state.insert(new);
    }
    state
}

pub fn part1<'a, I, S>(lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let state = directions()
        .take(10)
        .fold(parse(lines), |state, directions| step(&state, &directions));
    let (min_x, max_x, min_y, max_y) = state.iter().fold(
        (isize::MAX, isize::MIN, isize::MAX, isize::MIN),
        |(min_x, max_x, min_y, max_y), &(x, y)| {
            (min(min_x, x), max(max_x, x), min(min_y, y), max(max_y, y))
        },
    );
    (max_x - min_x + 1) as usize * (max_y - min_y + 1) as usize - state.len()
}

pub fn part2<'a, I, S>(lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    directions()
        .scan(parse(lines), |state, directions| {
            let next = step(state, &directions);
            if state != &next {
                *state = next;
                Some(())
            } else {
                None
            }
        })
        .count()
        + 1
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &[
        "....#..", "..###.#", "#...#.#", ".#...##", "#.###..", "##.#.##", ".#..#..",
    ];

    #[test]
    fn part1_examples() {
        assert_eq!(110, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(20, part2(EXAMPLE));
    }
}
