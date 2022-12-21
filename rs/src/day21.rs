use std::collections::BTreeMap;
use std::ops::{Add, Div, Mul, Neg, Rem, Sub};
use std::str::FromStr;

enum Operation {
    Add,
    Sub,
    Mul,
    Div,
}
impl FromStr for Operation {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+" => Ok(Self::Add),
            "-" => Ok(Self::Sub),
            "*" => Ok(Self::Mul),
            "/" => Ok(Self::Div),
            _ => Err(()),
        }
    }
}

enum Expr<T, R> {
    Literal(T),
    Binary(R, Operation, R),
}

fn parse<'a, I, S>(lines: I) -> Option<BTreeMap<&'a str, Expr<isize, &'a str>>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    lines
        .into_iter()
        .map(|line| {
            let (name, definition) = line.as_ref().split_once(": ")?;
            match definition.parse() {
                Ok(value) => Some((name, Expr::Literal(value))),
                _ => {
                    let mut iter = definition.split(' ');
                    Some((
                        name,
                        Expr::Binary(iter.next()?, iter.next()?.parse().ok()?, iter.next()?),
                    ))
                    .filter(|_| iter.next().is_none())
                }
            }
        })
        .collect()
}

pub fn part1<'a, I, S>(lines: I) -> Option<isize>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let definitions = parse(lines)?;
    fn eval(definitions: &BTreeMap<&str, Expr<isize, &str>>, name: &str) -> Option<isize> {
        match definitions.get(name)? {
            Expr::Literal(value) => Some(*value),
            Expr::Binary(lhs, op, rhs) => {
                let lhs = eval(definitions, lhs)?;
                let rhs = eval(definitions, rhs)?;
                match op {
                    Operation::Add => lhs.checked_add(rhs),
                    Operation::Sub => lhs.checked_sub(rhs),
                    Operation::Mul => lhs.checked_mul(rhs),
                    Operation::Div => lhs.checked_div(rhs),
                }
            }
        }
    }
    eval(&definitions, "root")
}

#[derive(Clone, Copy, Eq, PartialEq)]
struct Ratio<T> {
    numerator: T,
    denominator: T,
}
impl<T> Add<Ratio<T>> for Ratio<T>
where
    T: Add<T, Output = T>
        + Copy
        + Div<T, Output = T>
        + From<bool>
        + Mul<T, Output = T>
        + Neg<Output = T>
        + Ord
        + Rem<T, Output = T>,
{
    type Output = Self;
    fn add(self, rhs: Ratio<T>) -> Self::Output {
        let gcd1 = gcd(self.denominator, rhs.denominator);
        let multiplier = self.denominator / gcd1;
        let numerator = self.numerator * (rhs.denominator / gcd1) + rhs.numerator * multiplier;
        let denominator = multiplier * rhs.denominator;
        let gcd2 = gcd(numerator, denominator);
        Ratio {
            numerator: numerator / gcd2,
            denominator: denominator / gcd2,
        }
    }
}
impl<T> Sub<Ratio<T>> for Ratio<T>
where
    T: Copy
        + Div<T, Output = T>
        + From<bool>
        + Mul<T, Output = T>
        + Neg<Output = T>
        + Ord
        + Rem<T, Output = T>
        + Sub<T, Output = T>,
{
    type Output = Self;
    fn sub(self, rhs: Ratio<T>) -> Self::Output {
        let gcd1 = gcd(self.denominator, rhs.denominator);
        let multiplier = self.denominator / gcd1;
        let numerator = self.numerator * (rhs.denominator / gcd1) - rhs.numerator * multiplier;
        let denominator = multiplier * rhs.denominator;
        let gcd2 = gcd(numerator, denominator);
        Ratio {
            numerator: numerator / gcd2,
            denominator: denominator / gcd2,
        }
    }
}
impl<T> Mul<Ratio<T>> for Ratio<T>
where
    T: Copy
        + Div<T, Output = T>
        + From<bool>
        + Mul<T, Output = T>
        + Neg<Output = T>
        + Ord
        + Rem<T, Output = T>,
{
    type Output = Self;
    fn mul(self, rhs: Ratio<T>) -> Self::Output {
        let gcd1 = gcd(self.numerator, rhs.denominator);
        let gcd2 = gcd(rhs.numerator, self.denominator);
        Ratio {
            numerator: (self.numerator / gcd1) * (rhs.numerator / gcd2),
            denominator: (self.denominator / gcd2) * (rhs.denominator / gcd1),
        }
    }
}
impl<T> Div<Ratio<T>> for Ratio<T>
where
    T: Copy
        + Div<T, Output = T>
        + From<bool>
        + Mul<T, Output = T>
        + Neg<Output = T>
        + Ord
        + Rem<T, Output = T>,
{
    type Output = Self;
    fn div(self, rhs: Ratio<T>) -> Self::Output {
        assert!(rhs.numerator != false.into());
        let gcd1 = gcd(self.numerator, rhs.numerator);
        let gcd2 = gcd(rhs.denominator, self.denominator);
        Ratio {
            numerator: (self.numerator / gcd1) * (rhs.denominator / gcd2),
            denominator: (self.denominator / gcd2) * (rhs.numerator / gcd1),
        }
    }
}
impl<T: From<bool>> From<T> for Ratio<T> {
    fn from(value: T) -> Self {
        Ratio {
            numerator: value,
            denominator: true.into(),
        }
    }
}
impl TryFrom<Ratio<isize>> for isize {
    type Error = ();
    fn try_from(value: Ratio<isize>) -> Result<Self, Self::Error> {
        if value.denominator == 1 {
            Ok(value.numerator)
        } else {
            Err(())
        }
    }
}

fn gcd<T: Copy + From<bool> + Neg<Output = T> + Ord + Rem<T, Output = T>>(a: T, b: T) -> T {
    let mut x = a;
    let mut y = b;
    while y != false.into() {
        let tmp = x;
        x = y;
        y = tmp % y;
    }
    if (x < false.into()) != (b < false.into()) {
        -x
    } else {
        x
    }
}

pub fn part2<'a, I, S>(lines: I) -> Option<isize>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let definitions = parse(lines)?;
    fn eval(
        definitions: &BTreeMap<&str, Expr<isize, &str>>,
        name: &str,
    ) -> Option<(Ratio<isize>, Ratio<isize>)> {
        if name == "humn" {
            return Some((1.into(), 0.into()));
        }
        match definitions.get(name)? {
            Expr::Literal(value) => Some((0.into(), (*value).into())),
            Expr::Binary(lhs, op, rhs) => {
                let lhs = eval(definitions, lhs)?;
                let rhs = eval(definitions, rhs)?;
                match op {
                    Operation::Add => Some((lhs.0 + rhs.0, lhs.1 + rhs.1)),
                    Operation::Sub => Some((lhs.0 - rhs.0, lhs.1 - rhs.1)),
                    Operation::Mul => {
                        if lhs.0 == 0.into() {
                            Some((lhs.1 * rhs.0, lhs.1 * rhs.1))
                        } else if rhs.0 == 0.into() {
                            Some((lhs.0 * rhs.1, lhs.1 * rhs.1))
                        } else {
                            None
                        }
                    }
                    Operation::Div => {
                        if rhs.0 == 0.into() {
                            Some((lhs.0 / rhs.1, lhs.1 / rhs.1))
                        } else {
                            None
                        }
                    }
                }
            }
        }
    }
    let &Expr::Binary(lhs, _, rhs) = definitions.get("root")? else { return None };
    let lhs = eval(&definitions, lhs)?;
    let rhs = eval(&definitions, rhs)?;
    ((lhs.1 - rhs.1) / (rhs.0 - lhs.0)).try_into().ok()
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &[
        "root: pppw + sjmn",
        "dbpl: 5",
        "cczh: sllz + lgvd",
        "zczc: 2",
        "ptdq: humn - dvpt",
        "dvpt: 3",
        "lfqf: 4",
        "humn: 5",
        "ljgn: 2",
        "sjmn: drzm * dbpl",
        "sllz: 4",
        "pppw: cczh / lfqf",
        "lgvd: ljgn * ptdq",
        "drzm: hmdt - zczc",
        "hmdt: 32",
    ];

    #[test]
    fn part1_examples() {
        assert_eq!(Some(152), part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(Some(301), part2(EXAMPLE));
    }
}
