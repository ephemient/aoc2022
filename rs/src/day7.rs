use std::collections::HashMap;

fn parse<'a, I, S>(lines: I) -> HashMap<String, usize>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut sizes = [("".to_string(), 0)]
        .into_iter()
        .collect::<HashMap<String, usize>>();
    let mut cwd = "".to_string();
    for line in lines {
        let line = line.as_ref();
        if let Some(dir) = line.strip_prefix("$ cd ") {
            cwd = match dir {
                "/" => "".to_string(),
                ".." => cwd.rsplit_once('/').map_or("", |(cwd, _)| cwd).to_string(),
                _ => {
                    if cwd.is_empty() {
                        dir.to_string()
                    } else {
                        format!("{}/{}", cwd, dir)
                    }
                }
            };
        } else if let Some(size) = line
            .find(|c: char| !c.is_ascii_digit())
            .and_then(|i| line[..i].parse::<usize>().ok())
        {
            let mut path = cwd.to_string();
            loop {
                sizes
                    .entry(path.to_string())
                    .and_modify(|s| *s += size)
                    .or_insert(size);
                if path.is_empty() {
                    break;
                }
                path.drain(path.rfind('/').unwrap_or(0)..);
            }
        }
    }
    sizes
}

pub fn part1<'a, I, S>(lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    parse(lines)
        .into_values()
        .filter(|&size| size <= 100000)
        .sum()
}

pub fn part2<'a, I, S>(lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let sizes = parse(lines);
    let total = sizes[""];
    sizes
        .into_values()
        .filter(|size| 70000000 - (total - size) >= 30000000)
        .min()
        .unwrap_or(total)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &[
        "$ cd /",
        "$ ls",
        "dir a",
        "14848514 b.txt",
        "8504156 c.dat",
        "dir d",
        "$ cd a",
        "$ ls",
        "dir e",
        "29116 f",
        "2557 g",
        "62596 h.lst",
        "$ cd e",
        "$ ls",
        "584 i",
        "$ cd ..",
        "$ cd ..",
        "$ cd d",
        "$ ls",
        "4060174 j",
        "8033020 d.log",
        "5626152 d.ext",
        "7214296 k",
    ];

    #[test]
    fn part1_examples() {
        assert_eq!(95437, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(24933642, part2(EXAMPLE));
    }
}
