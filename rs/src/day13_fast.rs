use super::util::iter_chunks;
use std::cmp::Ordering;
use std::iter;
use std::marker::PhantomData;
use std::str::FromStr;

#[derive(Clone, Copy, Eq, PartialEq)]
enum Token<T> {
    Open,
    Close,
    Int(T),
}

struct TokenIterator<I, T> {
    chars: I,
    buf: Option<char>,
    _marker: PhantomData<T>,
}
impl<I: Iterator<Item = char>, T: FromStr> Iterator for TokenIterator<I, T> {
    type Item = Token<T>;
    fn next(&mut self) -> Option<Self::Item> {
        while let Some(char) = self.buf.take().or_else(|| self.chars.next()) {
            match char {
                ',' => continue,
                '0'..='9' => {
                    let digits = iter::once(char)
                        .chain(iter::from_fn(|| {
                            self.buf = self.chars.next();
                            self.buf.filter(|c| ('0'..='9').contains(c))
                        }))
                        .collect::<String>();
                    return Some(Token::Int(digits.parse().ok()?));
                }
                '[' => return Some(Token::Open),
                ']' => return Some(Token::Close),
                _ => break,
            }
        }
        None
    }
}

fn tokenize<I, T>(iter: I) -> TokenIterator<I, T> {
    TokenIterator {
        chars: iter,
        buf: None,
        _marker: PhantomData,
    }
}
fn compare<T: Ord, I: Iterator<Item = Token<T>>, J: Iterator<Item = Token<T>>>(
    mut lhs: I,
    mut rhs: J,
) -> Ordering {
    loop {
        match (
            lhs.next().unwrap_or(Token::Close),
            rhs.next().unwrap_or(Token::Close),
        ) {
            (Token::Close, Token::Close) => {}
            (Token::Close, _) => return Ordering::Less,
            (_, Token::Close) => return Ordering::Greater,
            (Token::Int(a), Token::Int(b)) => {
                let ordering = a.cmp(&b);
                if ordering != Ordering::Equal {
                    return ordering;
                }
            }
            (Token::Int(a), b) => {
                let Some((n, b)) = iter::once(b)
                    .chain(rhs.by_ref())
                    .enumerate()
                    .find(|(_, b)| b != &Token::Open)
                    .and_then(|(n, b)| match b {
                        Token::Int(b) => Some((n, b)),
                        _ => None,
                    }) else { return Ordering::Greater };
                let ordering = a.cmp(&b);
                if ordering != Ordering::Equal {
                    return ordering;
                }
                for _ in 0..n {
                    if rhs.next() != Some(Token::Close) {
                        return Ordering::Less;
                    }
                }
            }
            (a, Token::Int(b)) => {
                let Some((n, a)) = iter::once(a)
                    .chain(lhs.by_ref())
                    .enumerate()
                    .find(|(_, a)| a != &Token::Open)
                    .and_then(|(n, a)| match a {
                        Token::Int(a) => Some((n, a)),
                        _ => None,
                    }) else { return Ordering::Less };
                let ordering = a.cmp(&b);
                if ordering != Ordering::Equal {
                    return ordering;
                }
                for _ in 0..n {
                    if lhs.next() != Some(Token::Close) {
                        return Ordering::Greater;
                    }
                }
            }
            (a, b) => assert!(a == b),
        }
    }
}

pub fn part1<'a, I, S>(lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    iter_chunks(lines.into_iter())
        .enumerate()
        .filter(|(_, [a, b, _])| {
            compare(
                tokenize::<_, usize>(a.as_ref().chars()),
                tokenize(b.as_ref().chars()),
            ) <= Ordering::Equal
        })
        .map(|(i, _)| i + 1)
        .sum()
}

pub fn part2<'a, I, S>(lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let a = [
        Token::Open,
        Token::Open,
        Token::Int(2usize),
        Token::Close,
        Token::Close,
    ];
    let b = [
        Token::Open,
        Token::Open,
        Token::Int(6usize),
        Token::Close,
        Token::Close,
    ];
    let (x, y) = lines
        .into_iter()
        .map(|line| line.as_ref())
        .filter(|line| !line.is_empty())
        .fold((1, 1), |(x, y), line| {
            if compare(tokenize(line.chars()), a.iter().copied()) < Ordering::Equal {
                (x + 1, y)
            } else if compare(tokenize(line.chars()), b.iter().copied()) < Ordering::Equal {
                (x, y + 1)
            } else {
                (x, y)
            }
        });
    x * (x + y)
}
