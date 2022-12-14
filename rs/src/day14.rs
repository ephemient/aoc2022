use super::util::iter_pairs;
use std::cmp::{max, min};

struct Bitmap {
    data: Vec<bool>,
    width: usize,
}
impl Bitmap {
    fn new(width: usize, height: usize) -> Self {
        Bitmap {
            data: vec![false; width * height],
            width,
        }
    }

    fn contains(&self, x: usize, y: usize) -> bool {
        assert!((0..self.width).contains(&x));
        self.data[x + y * self.width]
    }

    fn add(&mut self, x: usize, y: usize) {
        assert!((0..self.width).contains(&x));
        self.data[x + y * self.width] = true;
    }
}

fn parse<'a, I, S>(lines: I) -> Option<(Bitmap, usize)>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let segments = lines
        .into_iter()
        .flat_map(|line| {
            iter_pairs(line.as_ref().split(" -> ").filter_map(|pair| {
                let (x, y) = pair.split_once(',')?;
                Some((x.parse().ok()?, y.parse().ok()?))
            }))
        })
        .collect::<Vec<_>>();
    let max_x = segments
        .iter()
        .map(|((x1, _), (x2, _))| max(*x1, *x2))
        .max()?;
    let max_y = segments
        .iter()
        .map(|((_, y1), (_, y2))| max(*y1, *y2))
        .max()?;
    let width = max(max_x + 1, 500 + max_y + 2);
    let mut bitmap = Bitmap::new(width, max_y + 2);
    for ((x1, y1), (x2, y2)) in segments {
        for x in min(x1, x2)..=max(x1, x2) {
            for y in min(y1, y2)..=max(y1, y2) {
                bitmap.add(x, y)
            }
        }
    }
    Some((bitmap, max_y))
}

enum FillState {
    Pre(usize, usize),
    Post(usize, usize),
}
struct FillIterator {
    blocks: Bitmap,
    max_y: usize,
    stack: Vec<FillState>,
}
impl FillIterator {
    fn new(blocks: Bitmap, max_y: usize) -> Self {
        FillIterator {
            blocks,
            max_y,
            stack: vec![FillState::Pre(500, 0)],
        }
    }
}
impl Iterator for FillIterator {
    type Item = (usize, usize);
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let state = self.stack.pop()?;
            match state {
                FillState::Pre(x, y) => {
                    if self.blocks.contains(x, y) {
                        continue;
                    }
                    self.stack.push(FillState::Post(x, y));
                    if y <= self.max_y {
                        self.stack.extend([
                            FillState::Pre(x + 1, y + 1),
                            FillState::Pre(x - 1, y + 1),
                            FillState::Pre(x, y + 1),
                        ]);
                    }
                }
                FillState::Post(x, y) => {
                    self.blocks.add(x, y);
                    return Some((x, y));
                }
            }
        }
    }
}

pub fn both_parts<'a, I, S>(lines: I) -> (usize, usize)
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let Some((blocks, max_y)) = parse(lines) else { return (0, 0) };
    FillIterator::new(blocks, max_y)
        .enumerate()
        .scan(None, |part1, (i, (_, y))| {
            if y >= max_y && part1.is_none() {
                *part1 = Some(i);
            }
            Some((part1.unwrap_or(i + 1), i + 1))
        })
        .last()
        .unwrap_or((0, 0))
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
        assert_eq!(24, both_parts(EXAMPLE).0);
    }

    #[test]
    fn part2_examples() {
        assert_eq!(93, both_parts(EXAMPLE).1);
    }
}
