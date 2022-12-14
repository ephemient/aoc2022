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

fn fall(blocks: &mut Bitmap, max_y: usize) -> Result<(usize, usize), usize> {
    let mut x = 500;
    for y in 0..max_y {
        if !blocks.contains(x, y + 1) {
        } else if !blocks.contains(x - 1, y + 1) {
            x -= 1
        } else if !blocks.contains(x + 1, y + 1) {
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
    let Some((mut blocks, max_y)) = parse(lines) else { return 0 };
    let mut count = 0;
    while let Ok((x, y)) = fall(&mut blocks, max_y) {
        blocks.add(x, y);
        count += 1;
    }
    count
}

pub fn part2<'a, I, S>(lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let Some((mut blocks, max_y)) = parse(lines) else { return 0 };
    let mut count = 0;
    while !blocks.contains(500, 0) {
        let (x, y) = fall(&mut blocks, max_y + 1).unwrap_or_else(|x| (x, max_y + 1));
        blocks.add(x, y);
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
