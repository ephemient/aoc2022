use std::cmp::min;
use std::collections::BTreeMap;

#[derive(Clone, Copy, Debug)]
enum Move {
    Steps(usize),
    Left,
    Right,
}

fn parse<'a, I, S>(lines: I) -> Option<(Vec<&'a [u8]>, Vec<Move>)>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut iter = lines.into_iter();
    let mut board = vec![];
    for line in iter.by_ref() {
        let line = line.as_ref();
        if line.is_empty() {
            break;
        }
        board.push(line.as_bytes());
    }
    let mut moves = vec![];
    let mut line = iter.next()?.as_ref();
    while !line.is_empty() {
        if let Some(tail) = line.strip_prefix('L') {
            moves.push(Move::Left);
            line = tail;
        } else if let Some(tail) = line.strip_prefix('R') {
            moves.push(Move::Right);
            line = tail;
        } else {
            let (digits, tail) = line.split_at(
                line.find(|c: char| !c.is_ascii_digit())
                    .unwrap_or(line.len()),
            );
            moves.push(Move::Steps(digits.parse().ok()?));
            line = tail;
        }
    }
    Some((board, moves))
}

struct Perimeter<'a> {
    board: &'a [&'a [u8]],
    initial: (usize, usize, u8),
    next: Option<(usize, usize, u8)>,
}
fn perimeter<'a>(board: &'a [&'a [u8]]) -> Option<Perimeter<'a>> {
    let initial = (
        board
            .first()?
            .iter()
            .enumerate()
            .find(|(_, &b)| b == b'.')?
            .0,
        0,
        0,
    );
    Some(Perimeter {
        board,
        initial,
        next: Some(initial),
    })
}
impl<'a> Iterator for Perimeter<'a> {
    type Item = (usize, usize, u8);
    fn next(&mut self) -> Option<Self::Item> {
        let current @ (x, y, d) = self.next?;
        let forward = match d {
            0 => Some((x + 1, y)),
            1 => Some((x, y + 1)),
            2 => x.checked_sub(1).map(|x| (x, y)),
            3 => y.checked_sub(1).map(|y| (x, y)),
            _ => unreachable!(),
        };
        self.next = if forward
            .and_then(|(x, y)| Some(*self.board.get(y)?.get(x)?))
            .unwrap_or(b' ')
            == b' '
        {
            Some((x, y, (d + 1) % 4))
        } else {
            let left = match d {
                0 => y.checked_sub(1).map(|y| (x + 1, y)),
                1 => Some((x + 1, y + 1)),
                2 => x.checked_sub(1).map(|x| (x, y + 1)),
                3 => x.checked_sub(1).zip(y.checked_sub(1)),
                _ => unreachable!(),
            };
            if left
                .and_then(|(x, y)| Some(*self.board.get(y)?.get(x)?))
                .unwrap_or(b' ')
                == b' '
            {
                forward.map(|(x, y)| (x, y, d))
            } else {
                left.map(|(x, y)| (x, y, (d + 3) % 4))
            }
        }
        .filter(|state| state != &self.initial);
        Some(current)
    }
}

fn run(
    board: &[&[u8]],
    moves: &[Move],
    warps: &BTreeMap<(usize, usize, u8), (usize, usize, u8)>,
) -> Option<usize> {
    let (x, y, d) = moves.iter().fold(
        (
            board
                .first()?
                .iter()
                .enumerate()
                .find(|(_, &b)| b == b'.')?
                .0,
            0,
            0u8,
        ),
        |(x, y, d), &r#move| match r#move {
            Move::Steps(n) => {
                let mut pos = (x, y, d);
                for _ in 0..n {
                    let Some(next) = warps
                        .get(&pos)
                        .copied()
                        .or_else(|| {
                            match pos.2 {
                                0 => Some((pos.0 + 1, pos.1, pos.2)),
                                1 => Some((pos.0, pos.1 + 1, pos.2)),
                                2 => pos.0.checked_sub(1).map(|x| (x, pos.1, pos.2)),
                                3 => pos.1.checked_sub(1).map(|y| (pos.0, y, pos.2)),
                                _ => unreachable!(),
                            }
                        })
                        .filter(|&(x, y, _)| {
                            board.get(y).and_then(|line| line.get(x)) == Some(&b'.')
                        })
                        else { return pos };
                    pos = next;
                }
                pos
            }
            Move::Left => (x, y, (d + 3) % 4),
            Move::Right => (x, y, (d + 1) % 4),
        },
    );
    Some(1000 * (y + 1) + 4 * (x + 1) + d as usize)
}

pub fn part1<'a, I, S>(lines: I) -> Option<usize>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let (board, moves) = parse(lines)?;
    let warps = perimeter(&board)?
        .map(|(x, y, d)| match d {
            0 => board
                .iter()
                .enumerate()
                .rev()
                .find(|(_, &line)| line.get(x).filter(|&&b| b != b' ').is_some())
                .map(|(y2, _)| ((x, y, 3), (x, y2, 3))),
            1 => board.get(y).and_then(|&line| {
                line.iter()
                    .enumerate()
                    .find(|(_, &b)| b != b' ')
                    .map(|(x2, _)| ((x, y, 0), (x2, y, 0)))
            }),
            2 => board
                .iter()
                .enumerate()
                .find(|(_, &line)| line.get(x).filter(|&&b| b != b' ').is_some())
                .map(|(y2, _)| ((x, y, 1), (x, y2, 1))),
            3 => board.get(y).and_then(|&line| {
                line.iter()
                    .enumerate()
                    .rev()
                    .find(|(_, &b)| b != b' ')
                    .map(|(x2, _)| ((x, y, 2), (x2, y, 2)))
            }),
            _ => unreachable!(),
        })
        .collect::<Option<_>>()?;
    run(&board, &moves, &warps)
}

pub fn part2<'a, I, S>(lines: I) -> Option<usize>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let (board, moves) = parse(lines)?;
    let perimeter = perimeter(&board)?.collect::<Vec<_>>();
    let side_length = perimeter
        .iter()
        .map(|&(_, _, d)| Some(d))
        .chain(None)
        .fold((Some(0), 0, usize::MAX), |(prev, count, size), cur| {
            if prev == cur {
                (cur, count + 1, size)
            } else {
                (cur, 1, min(size, count))
            }
        })
        .2;
    let mut unpaired_edges = perimeter
        .iter()
        .enumerate()
        .step_by(side_length)
        .map(|(i, &(_, _, d))| (i, d))
        .collect::<Vec<_>>();
    let mut paired_edges = vec![];
    while !unpaired_edges.is_empty() {
        let mut i = 0;
        while i + 1 < unpaired_edges.len() {
            if unpaired_edges[i].1 == (unpaired_edges[i + 1].1 + 1) % 4 {
                paired_edges.push((unpaired_edges[i].0, unpaired_edges[i + 1].0));
                unpaired_edges.drain(i..i + 2);
                for (_, d) in &mut unpaired_edges[i..] {
                    *d = (*d + 3) % 4;
                }
            } else {
                i += 1;
            }
        }
    }
    let mut warps = BTreeMap::new();
    for (i, j) in paired_edges {
        for (&(x1, y1, d1), &(x2, y2, d2)) in perimeter[i..i + side_length]
            .iter()
            .zip(perimeter[j..j + side_length].iter().rev())
        {
            warps.insert((x1, y1, (d1 + 3) % 4), (x2, y2, (d2 + 1) % 4));
            warps.insert((x2, y2, (d2 + 3) % 4), (x1, y1, (d1 + 1) % 4));
        }
    }
    run(&board, &moves, &warps)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &[
        "        ...#",
        "        .#..",
        "        #...",
        "        ....",
        "...#.......#",
        "........#...",
        "..#....#....",
        "..........#.",
        "        ...#....",
        "        .....#..",
        "        .#......",
        "        ......#.",
        "",
        "10R5L5R10L4R5L5",
    ];

    #[test]
    fn part1_examples() {
        assert_eq!(Some(6032), part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(Some(5031), part2(EXAMPLE));
    }
}
