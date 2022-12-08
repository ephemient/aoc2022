use std::collections::VecDeque;
use std::iter::Enumerate;

fn scan_visibility<I: Iterator>(iter: I) -> ScanVisibility<I> {
    ScanVisibility { iter, max: None }
}
struct ScanVisibility<I: Iterator> {
    iter: I,
    max: Option<<I as Iterator>::Item>,
}
impl<I> Iterator for ScanVisibility<I>
where
    I: Iterator,
    <I as Iterator>::Item: Ord,
{
    type Item = bool;
    fn next(&mut self) -> Option<Self::Item> {
        let item = self.iter.next()?;
        if self.max.as_ref().filter(|&max| max >= &item).is_none() {
            self.max = Some(item);
            Some(true)
        } else {
            Some(false)
        }
    }
}

pub fn part1<'a, I, S>(lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let lines = lines
        .into_iter()
        .map(|line| line.as_ref().to_string())
        .collect::<Vec<_>>();
    let width = lines.iter().map(|line| line.len()).max().unwrap_or(0);
    let mut visibilities = lines
        .iter()
        .map(|line| scan_visibility(line.chars()).collect())
        .collect::<Vec<Vec<_>>>();
    for (line, row) in lines.iter().zip(visibilities.iter_mut()) {
        for (value, result) in scan_visibility(line.chars().rev()).zip(row.iter_mut().rev()) {
            if value {
                *result = true;
            }
        }
    }
    for col in 0..width {
        for (row, value) in
            scan_visibility(lines.iter().filter_map(|line| line.chars().nth(col))).enumerate()
        {
            if value {
                visibilities[row][col] = true;
            }
        }
        for (row, value) in
            scan_visibility(lines.iter().rev().filter_map(|line| line.chars().nth(col))).enumerate()
        {
            if value {
                visibilities[lines.len() - row - 1][col] = true;
            }
        }
    }
    visibilities
        .into_iter()
        .map(|row| row.into_iter().filter(|b| *b).count())
        .sum()
}

fn scan_score<I: Iterator>(iter: I) -> ScanScore<I> {
    ScanScore {
        iter: iter.enumerate(),
        horizon: VecDeque::new(),
    }
}
struct ScanScore<I: Iterator> {
    iter: Enumerate<I>,
    horizon: VecDeque<(<I as Iterator>::Item, usize)>,
}
impl<I> Iterator for ScanScore<I>
where
    I: Iterator,
    <I as Iterator>::Item: Clone + Ord,
{
    type Item = usize;
    fn next(&mut self) -> Option<Self::Item> {
        let (index, item) = self.iter.next()?;
        self.horizon.drain(
            ..self
                .horizon
                .binary_search_by(|(key, _)| key.cmp(&item))
                .map_or_else(|x| x, |x| x),
        );
        let prev = self.horizon.front().map_or(0, |(_, prev)| *prev);
        match self.horizon.front_mut().filter(|(key, _)| key == &item) {
            Some((_, e)) => *e = index,
            _ => self.horizon.push_front((item, index)),
        }
        Some(index - prev)
    }
}

pub fn part2<'a, I, S>(lines: I) -> Option<usize>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let lines = lines
        .into_iter()
        .map(|line| line.as_ref().to_string())
        .collect::<Vec<_>>();
    let width = lines.iter().map(|line| line.len()).max().unwrap_or(0);
    let mut scores = lines
        .iter()
        .map(|line| scan_score(line.chars()).collect())
        .collect::<Vec<Vec<_>>>();
    for (line, row) in lines.iter().zip(scores.iter_mut()) {
        for (value, result) in scan_score(line.chars().rev()).zip(row.iter_mut().rev()) {
            *result *= value;
        }
    }
    for col in 0..width {
        for (row, value) in
            scan_score(lines.iter().filter_map(|line| line.chars().nth(col))).enumerate()
        {
            scores[row][col] *= value;
        }
        for (row, value) in
            scan_score(lines.iter().rev().filter_map(|line| line.chars().nth(col))).enumerate()
        {
            scores[lines.len() - row - 1][col] *= value;
        }
    }
    scores
        .into_iter()
        .filter_map(|row| row.into_iter().max())
        .max()
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &["30373", "25512", "65332", "33549", "35390"];

    #[test]
    fn part1_examples() {
        assert_eq!(21, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(Some(8), part2(EXAMPLE));
    }
}
