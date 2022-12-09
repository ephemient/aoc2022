use std::collections::HashSet;

fn moves<'a, I, S>(lines: I) -> Moves<I>
where
    I: Iterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    Moves {
        lines,
        pos: (0, 0),
        dest: Some((0, 0)),
    }
}
struct Moves<I: Iterator> {
    lines: I,
    pos: (i32, i32),
    dest: Option<(i32, i32)>,
}
impl<'a, I, S> Iterator for Moves<I>
where
    I: Iterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    type Item = (i32, i32);
    fn next(&mut self) -> Option<Self::Item> {
        let pos = self.pos;
        let dest = self.dest.or_else(|| {
            self.lines
                .by_ref()
                .filter_map(|line| {
                    let line = line.as_ref();
                    let mut iter = line.split_ascii_whitespace();
                    let dir = iter.next()?;
                    let count = iter.next()?.parse::<i32>().ok()?;
                    match dir {
                        "L" => Some((pos.0 - count, pos.1)),
                        "R" => Some((pos.0 + count, pos.1)),
                        "U" => Some((pos.0, pos.1 - count)),
                        "D" => Some((pos.0, pos.1 + count)),
                        _ => None,
                    }
                })
                .find(|&dest| pos != dest)
        })?;
        self.pos = (
            pos.0 + (dest.0 - pos.0).signum(),
            pos.1 + (dest.1 - pos.1).signum(),
        );
        self.dest = if self.pos == dest { None } else { Some(dest) };
        Some(self.pos)
    }
}

fn follow<I: Iterator>(iter: I) -> Follow<I> {
    Follow { iter, pos: None }
}
struct Follow<I: Iterator> {
    iter: I,
    pos: Option<(i32, i32)>,
}
impl<I: Iterator<Item = (i32, i32)>> Iterator for Follow<I> {
    type Item = (i32, i32);
    fn next(&mut self) -> Option<Self::Item> {
        let Some(tail) = self.pos else {
            self.pos = Some((0, 0));
            return Some((0, 0));
        };
        let head = self
            .iter
            .find(|head| (head.0 - tail.0).abs() > 1 || (head.1 - tail.1).abs() > 1)?;
        let delta = (head.0 - tail.0, head.1 - tail.1);
        let pos = (
            if delta.0.abs() >= delta.1.abs() {
                head.0 - delta.0.signum()
            } else {
                head.0
            },
            if delta.0.abs() <= delta.1.abs() {
                head.1 - delta.1.signum()
            } else {
                head.1
            },
        );
        self.pos = Some(pos);
        Some(pos)
    }
}

pub fn part1<'a, I, S>(lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    follow(moves(lines.into_iter()))
        .collect::<HashSet<_>>()
        .len()
}

pub fn part2<'a, I, S>(lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    follow(follow(follow(follow(follow(follow(follow(follow(
        follow(moves(lines.into_iter())),
    ))))))))
    .collect::<HashSet<_>>()
    .len()
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE_1: &[&str] = &["R 4", "U 4", "L 3", "D 1", "R 4", "D 1", "L 5", "R 2"];
    static EXAMPLE_2: &[&str] = &["R 5", "U 8", "L 8", "D 3", "R 17", "D 10", "L 25", "U 20"];

    #[test]
    fn part1_examples() {
        assert_eq!(13, part1(EXAMPLE_1));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(1, part2(EXAMPLE_1));
        assert_eq!(36, part2(EXAMPLE_2));
    }
}
