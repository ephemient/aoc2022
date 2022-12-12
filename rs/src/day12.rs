use std::cmp::min;
use std::collections::{HashSet, VecDeque};
use std::hash::Hash;
use std::iter;

struct Bfs<Item, F> {
    queue: VecDeque<(Item, usize)>,
    visited: HashSet<Item>,
    neighbors: F,
}
impl<Item: Eq + Hash, F: FnMut(&Item) -> I, I: IntoIterator> Bfs<Item, F>
where
    <I as IntoIterator>::IntoIter: Iterator<Item = Item>,
{
    fn new(start: Item, neighbors: F) -> Bfs<Item, F> {
        Bfs {
            queue: [(start, 0)].into_iter().collect(),
            visited: HashSet::new(),
            neighbors,
        }
    }
}
impl<Item: Clone + Eq + Hash, F: FnMut(&Item) -> I, I: IntoIterator> Iterator for Bfs<Item, F>
where
    <I as IntoIterator>::IntoIter: Iterator<Item = Item>,
{
    type Item = (Item, usize);
    fn next(&mut self) -> Option<Self::Item> {
        let (item, depth) = self.queue.pop_front()?;
        for neighbor in (self.neighbors)(&item) {
            if self.visited.insert(neighbor.clone()) {
                self.queue.push_back((neighbor, depth + 1));
            }
        }
        Some((item, depth))
    }
}

pub fn part1<'a, I, S>(lines: I) -> Option<usize>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let lines = lines
        .into_iter()
        .map(|line| line.as_ref().chars().collect())
        .collect::<Vec<Vec<_>>>();
    Bfs::new(
        lines.iter().enumerate().find_map(|(row, line)| {
            line.iter()
                .enumerate()
                .find(|(_, c)| c == &&'S')
                .map(|(col, _)| (row, col))
        })?,
        |&(row0, col0)| -> Vec<_> {
            let a = lines[row0][col0];
            (row0.saturating_sub(1)..min(row0 + 2, lines.len()))
                .flat_map(|row1| {
                    let line = &lines[row1];
                    if row0 == row1 {
                        col0.checked_sub(1)
                    } else {
                        None
                    }
                    .into_iter()
                    .chain(iter::once(if row0 == row1 { col0 + 1 } else { col0 }))
                    .filter(|col1| col1 < &line.len())
                    .map(move |col1| (row1, col1))
                })
                .filter(|&(row1, col1)| {
                    let b = lines[row1][col1];
                    if a == 'S' {
                        b == 'a'
                    } else if b == 'E' {
                        a == 'z'
                    } else {
                        b as i32 - a as i32 <= 1
                    }
                })
                .collect()
        },
    )
    .find(|((row, col), _)| lines[*row][*col] == 'E')
    .map(|(_, depth)| depth)
}

pub fn part2<'a, I, S>(lines: I) -> Option<usize>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let lines = lines
        .into_iter()
        .map(|line| line.as_ref().chars().collect())
        .collect::<Vec<Vec<_>>>();
    Bfs::new(
        lines.iter().enumerate().find_map(|(row, line)| {
            line.iter()
                .enumerate()
                .find(|(_, c)| c == &&'E')
                .map(|(col, _)| (row, col))
        })?,
        |&(row0, col0)| -> Vec<_> {
            let a = lines[row0][col0];
            (row0.saturating_sub(1)..min(row0 + 2, lines.len()))
                .flat_map(|row1| {
                    let line = &lines[row1];
                    if row0 == row1 {
                        col0.checked_sub(1)
                    } else {
                        None
                    }
                    .into_iter()
                    .chain(iter::once(if row0 == row1 { col0 + 1 } else { col0 }))
                    .filter(|col1| col1 < &line.len())
                    .map(move |col1| (row1, col1))
                })
                .filter(|&(row1, col1)| {
                    let b = lines[row1][col1];
                    if a == 'E' {
                        b == 'z'
                    } else {
                        a as i32 - b as i32 <= 1
                    }
                })
                .collect()
        },
    )
    .find(|((row, col), _)| lines[*row][*col] == 'a')
    .map(|(_, depth)| depth)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &["Sabqponm", "abcryxxl", "accszExk", "acctuvwj", "abdefghi"];

    #[test]
    fn part1_examples() {
        assert_eq!(Some(31), part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(Some(29), part2(EXAMPLE));
    }
}
