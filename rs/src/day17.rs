use smallvec::{smallvec, SmallVec};
use static_init::dynamic;
use std::collections::HashMap;

fn parse<'a, I, S>(lines: I) -> Vec<i8>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    lines
        .into_iter()
        .flat_map(|line| line.as_ref().chars())
        .filter_map(|char| match char {
            '<' => Some(-1),
            '>' => Some(1),
            _ => None,
        })
        .collect()
}

struct Rock {
    width: u8,
    rows: SmallVec<[u8; 4]>,
}
#[dynamic]
static ROCKS: [Rock; 5] = [
    Rock {
        width: 4,
        rows: smallvec![15],
    },
    Rock {
        width: 3,
        rows: smallvec![2, 7, 2],
    },
    Rock {
        width: 3,
        rows: smallvec![7, 4, 4],
    },
    Rock {
        width: 1,
        rows: smallvec![1, 1, 1, 1],
    },
    Rock {
        width: 2,
        rows: smallvec![3, 3],
    },
];

struct State {
    height: u64,
    visible: Vec<u8>,
}
impl State {
    fn contains(&self, x: u8, y: usize, rock: &Rock) -> bool {
        rock.rows
            .iter()
            .enumerate()
            .filter_map(|(dy, row)| Some(self.visible.get(y + dy)? & row << x))
            .any(|intersection| intersection != 0)
    }

    fn add(&self, x: u8, y: usize, rock: &Rock) -> State {
        assert!(y <= self.visible.len());
        let mut height = self.height;
        let mut visible = self.visible.clone();
        for (dy, row) in rock.rows.iter().enumerate() {
            if let Some(v) = visible.get_mut(y + dy) {
                *v |= row << x;
            } else {
                height += 1;
                visible.push(row << x);
            }
        }

        let mut seen = vec![0; visible.len() + 1];
        *seen.last_mut().unwrap() = 1;
        let mut queue = vec![visible.len() << 3];
        while let Some(pos) = queue.pop() {
            if pos < visible.len() << 3 && visible[pos >> 3] & 1 << (pos & 7) != 0 {
                continue;
            }
            for pos in [
                pos.checked_sub(8),
                pos.checked_sub(1).filter(|&pos| pos & 7 != 7),
                Some(pos + 1).filter(|&pos| pos & 7 != 7),
                Some(pos + 8).filter(|&pos| pos < seen.len() << 3),
            ] {
                let Some(pos) = pos else { continue };
                if seen[pos >> 3] & 1 << (pos & 7) == 0 {
                    seen[pos >> 3] |= 1 << (pos & 7);
                    queue.push(pos);
                }
            }
        }

        for (v, s) in visible.iter_mut().zip(seen.iter()) {
            *v &= s;
        }
        visible.drain(..visible.iter().enumerate().find(|(_, &v)| v != 0).unwrap().0);
        State { height, visible }
    }
}

fn solve(jet: &[i8], n: u64) -> u64 {
    let mut jet_index = 0;
    let mut state = State {
        height: 0,
        visible: vec![127],
    };
    let mut seen = HashMap::new();
    let mut heights = vec![];
    for i in 0..n {
        let rock_index = (i % ROCKS.len() as u64) as usize;
        if let Some(j) = seen.insert((rock_index, jet_index, state.visible.clone()), i) {
            let q = (n - j) / (i - j);
            let r = (n - j) % (i - j);
            return heights[(j + r) as usize] + q * (state.height - heights[j as usize]);
        }
        let rock = &ROCKS[rock_index];
        let (mut x, mut y) = (2u8, state.visible.len() + 3);
        while !state.contains(x, y, rock) {
            x = x
                .checked_add_signed(jet[jet_index])
                .filter(|&x| x + rock.width <= 7 && !state.contains(x, y, rock))
                .unwrap_or(x);
            jet_index = (jet_index + 1) % jet.len();
            y -= 1;
        }
        heights.push(state.height);
        state = state.add(x, y + 1, rock);
    }
    state.height
}

pub fn part1<'a, I, S>(lines: I) -> u64
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    solve(&parse(lines), 2022)
}

pub fn part2<'a, I, S>(lines: I) -> u64
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    solve(&parse(lines), 1000000000000)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &[">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"];

    #[test]
    fn part1_examples() {
        assert_eq!(3068, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(1514285714288, part2(EXAMPLE));
    }
}
