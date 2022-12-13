use std::cmp::Ordering;
use std::iter;
use std::str::FromStr;

#[derive(Eq, PartialEq)]
enum Packet<T> {
    Literal(T),
    List(Vec<Packet<T>>),
}
impl<T: FromStr> FromStr for Packet<T> {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut stack: Vec<Vec<Packet<T>>> = vec![];
        let mut chars = s.chars();
        let mut buf = None;
        while let Some(char) = buf.take().or_else(|| chars.next()) {
            match char {
                ',' => {}
                '0'..='9' => {
                    let digits = iter::once(char)
                        .chain(iter::from_fn(|| {
                            buf = chars.next();
                            buf.filter(|c| ('0'..='9').contains(c))
                        }))
                        .collect::<String>();
                    stack
                        .last_mut()
                        .ok_or(())?
                        .push(Packet::Literal(digits.parse::<T>().map_err(|_| ())?));
                }
                '[' => stack.push(vec![]),
                ']' => {
                    let list = Packet::List(stack.pop().ok_or(())?);
                    match stack.last_mut() {
                        None => return chars.next().map_or_else(|| Ok(list), |_| Err(())),
                        Some(last) => last.push(list),
                    }
                }
                _ => break,
            }
        }
        Err(())
    }
}
impl<T: PartialOrd> PartialOrd for Packet<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Packet::Literal(a), Packet::Literal(b)) => a.partial_cmp(b),
            (Packet::List(left), Packet::List(right)) => {
                lexicographical_partial_cmp(left.iter(), right.iter())
            }
            (left, Packet::List(right)) => {
                lexicographical_partial_cmp(iter::once(left), right.iter())
            }
            (Packet::List(left), right) => {
                lexicographical_partial_cmp(left.iter(), iter::once(right))
            }
        }
    }
}
impl<T: Ord> Ord for Packet<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Packet::Literal(a), Packet::Literal(b)) => a.cmp(b),
            (Packet::List(left), Packet::List(right)) => {
                lexicographical_cmp(left.iter(), right.iter())
            }
            (left, Packet::List(right)) => lexicographical_cmp(iter::once(left), right.iter()),
            (Packet::List(left), right) => lexicographical_cmp(left.iter(), iter::once(right)),
        }
    }
}
fn lexicographical_partial_cmp<
    Item: PartialOrd,
    I: Iterator<Item = Item>,
    J: Iterator<Item = Item>,
>(
    mut left: I,
    mut right: J,
) -> Option<Ordering> {
    loop {
        return match (left.next(), right.next()) {
            (None, None) => Some(Ordering::Equal),
            (None, Some(_)) => Some(Ordering::Less),
            (Some(_), None) => Some(Ordering::Greater),
            (Some(a), Some(b)) => match a.partial_cmp(&b)? {
                Ordering::Equal => continue,
                ordering => Some(ordering),
            },
        };
    }
}
fn lexicographical_cmp<Item: Ord, I: Iterator<Item = Item>, J: Iterator<Item = Item>>(
    mut left: I,
    mut right: J,
) -> Ordering {
    loop {
        return match (left.next(), right.next()) {
            (None, None) => Ordering::Equal,
            (None, Some(_)) => Ordering::Less,
            (Some(_), None) => Ordering::Greater,
            (Some(a), Some(b)) => match a.cmp(&b) {
                Ordering::Equal => continue,
                ordering => ordering,
            },
        };
    }
}

pub fn part1<'a, I, S>(lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut iter = lines.into_iter().fuse();
    iter::from_fn(|| {
        let left = iter.next()?.as_ref().parse::<Packet<usize>>().ok()?;
        let right = iter.next()?.as_ref().parse().ok()?;
        iter.next();
        Some(left <= right)
    })
    .enumerate()
    .filter_map(|(i, ok)| if ok { Some(i + 1) } else { None })
    .sum()
}

pub fn part2<'a, I, S>(lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let a = Packet::List(vec![Packet::List(vec![Packet::Literal(2usize)])]);
    let b = Packet::List(vec![Packet::List(vec![Packet::Literal(6)])]);
    let (x, y) = lines.into_iter().fold((1, 1), |(x, y), line| {
        let Ok(packet) = line.as_ref().parse::<Packet<_>>() else { return (x, y) };
        if packet < a {
            (x + 1, y)
        } else if packet < b {
            (x, y + 1)
        } else {
            (x, y)
        }
    });
    x * (x + y)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::day13_fast;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &[
        "[1,1,3,1,1]",
        "[1,1,5,1,1]",
        "",
        "[[1],[2,3,4]]",
        "[[1],4]",
        "",
        "[9]",
        "[[8,7,6]]",
        "",
        "[[4,4],4,4]",
        "[[4,4],4,4,4]",
        "",
        "[7,7,7,7]",
        "[7,7,7]",
        "",
        "[]",
        "[3]",
        "",
        "[[[]]]",
        "[[]]",
        "",
        "[1,[2,[3,[4,[5,6,7]]]],8,9]",
        "[1,[2,[3,[4,[5,6,0]]]],8,9]",
    ];

    #[test]
    fn part1_examples() {
        assert_eq!(13, part1(EXAMPLE));
    }

    #[test]
    fn part1_examples_fast() {
        assert_eq!(13, day13_fast::part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(140, part2(EXAMPLE));
    }

    #[test]
    fn part2_examples_fast() {
        assert_eq!(140, day13_fast::part2(EXAMPLE));
    }
}
