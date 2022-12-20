use std::cmp::Ordering;

use std::num::ParseIntError;

fn parse<'a, I, S>(lines: I) -> Result<Vec<(usize, isize)>, ParseIntError>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    lines
        .into_iter()
        .enumerate()
        .map(|(i, line)| Ok((i, line.as_ref().parse()?)))
        .collect()
}

fn mix(array: &mut [(usize, isize)]) -> Option<()> {
    for i in 0..array.len() {
        let (i, &a) = array.iter().enumerate().find(|(_, (j, _))| j == &i)?;
        let j = (i as isize + a.1).rem_euclid(array.len() as isize - 1) as usize;
        match i.cmp(&j) {
            Ordering::Less => array.copy_within(i + 1..j + 1, i),
            Ordering::Equal => {}
            Ordering::Greater => array.copy_within(j..i, j + 1),
        }
        array[j] = a;
    }
    Some(())
}

pub fn part1<'a, I, S>(lines: I) -> Option<isize>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut array = parse(lines).ok()?;
    mix(&mut array)?;
    let (i, _) = array.iter().enumerate().find(|(_, (_, x))| x == &0)?;
    Some(
        [1000, 2000, 3000]
            .iter()
            .map(|x| array[(i + x) % array.len()].1)
            .sum(),
    )
}

pub fn part2<'a, I, S>(lines: I) -> Option<isize>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut array = parse(lines).ok()?;
    for entry in &mut array {
        entry.1 *= 811589153;
    }
    for _ in 0..10 {
        mix(&mut array)?;
    }
    let (i, _) = array.iter().enumerate().find(|(_, (_, x))| x == &0)?;
    Some(
        [1000, 2000, 3000]
            .iter()
            .map(|x| array[(i + x) % array.len()].1)
            .sum(),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &["1", "2", "-3", "3", "-2", "0", "4"];

    #[test]
    fn part1_examples() {
        assert_eq!(Some(3), part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(Some(1623178306), part2(EXAMPLE));
    }
}
