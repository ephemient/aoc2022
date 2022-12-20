use std::cmp::{max, min};
use std::collections::{BTreeMap, BTreeSet, BinaryHeap, HashSet};
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::Add;

fn parse_line(line: &str) -> Option<(&str, usize, Vec<&str>)> {
    let line = line.strip_prefix("Valve ")?;
    let (src, line) = line.split_once(' ')?;
    let line = line.strip_prefix("has flow rate=")?;
    let (rate, line) = line.split_once(';')?;
    let rate = rate.parse().ok()?;
    let line = line.splitn(6, ' ').nth(5)?;
    let dsts = line.split(", ").collect();
    Some((src, rate, dsts))
}

fn distances<I, T, U>(adj: I) -> BTreeMap<(T, T), U>
where
    I: IntoIterator<Item = (T, T, U)>,
    T: Clone + Ord,
    U: Add<U, Output = U> + Copy + Default + Ord,
{
    let mut distances = BTreeMap::new();
    let mut keys = BTreeSet::new();
    for (src, dst, w) in adj {
        distances.insert((src.clone(), src.clone()), <U as Default>::default());
        distances.insert((dst.clone(), dst.clone()), <U as Default>::default());
        distances.insert((src.clone(), dst.clone()), w);
        keys.insert(src);
        keys.insert(dst);
    }
    for mid in &keys {
        for src in &keys {
            for dst in &keys {
                let Some(&x) = distances.get(&(src.clone(), mid.clone())) else { continue };
                let Some(&y) = distances.get(&(mid.clone(), dst.clone())) else { continue };
                distances
                    .entry((src.clone(), dst.clone()))
                    .and_modify(|z| *z = min(*z, x + y))
                    .or_insert_with(|| x + y);
            }
        }
    }
    distances
}

struct Search<T, R, F> {
    next: F,
    seen: HashSet<T>,
    best_estimate: Option<R>,
    heap: BinaryHeap<(R, T)>,
}
impl<T: Ord, R: Ord, F: FnMut(&T) -> (Option<R>, I), I> Search<T, R, F> {
    fn new(initial: (R, T), next: F) -> Self {
        Search {
            next,
            seen: HashSet::new(),
            best_estimate: None,
            heap: [initial].into(),
        }
    }
}
impl<T, R, F, I> Iterator for Search<T, R, F>
where
    T: Clone + Debug + Hash + Ord,
    R: Copy + Debug + Ord,
    F: FnMut(&T) -> (Option<R>, I),
    I: IntoIterator<Item = (R, T)>,
{
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        while let Some((r, t)) = self.heap.pop() {
            if !self.seen.insert(t.clone()) {
                continue;
            }
            self.best_estimate = max(self.best_estimate, Some(r));
            let (potential, nexts) = (self.next)(&t);
            if let Some(potential) = potential {
                if let Some(best_estimate) = self.best_estimate {
                    if potential < best_estimate {
                        continue;
                    }
                }
            }
            for next in nexts {
                if !self.seen.contains(&next.1) {
                    self.heap.push(next);
                }
            }
            return Some(t);
        }
        None
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct State<'a, const N: usize> {
    rooms: [(&'a str, usize); N],
    valves: BTreeSet<&'a str>,
    flow: usize,
    total: usize,
    time: usize,
}

fn solve<'a, const N: usize, const M: usize, I, S>(lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let graph = lines
        .into_iter()
        .filter_map(|line| {
            let (src, rate, dsts) = parse_line(line.as_ref())?;
            Some((src, (rate, dsts)))
        })
        .collect::<BTreeMap<_, _>>();
    let distances = distances(
        graph
            .iter()
            .flat_map(|(&src, (_, dsts))| dsts.iter().map(move |&dst| (src, dst, 1usize))),
    );
    let valves = graph
        .iter()
        .filter(|(_, (rate, _))| *rate > 0)
        .map(|(&src, _)| src)
        .collect();
    let initial_state = State {
        rooms: [("AA", 0); N],
        valves,
        flow: 0,
        total: 0,
        time: M,
    };
    Search::new((0, initial_state), |state| {
        let estimate = state.total + state.flow * state.time;
        let potential = estimate
            + state
                .valves
                .iter()
                .filter_map(|&valve| {
                    state
                        .rooms
                        .iter()
                        .filter_map(|&(room, age)| {
                            distances
                                .get(&(room, valve))
                                .and_then(|d| d.checked_sub(age))
                                .filter(|&d| d < state.time)
                                .map(|d| graph[room].0 * (state.time - d - 1))
                        })
                        .max()
                })
                .sum::<usize>();
        let mut moves = BTreeMap::new();
        for &valve in &state.valves {
            for (i, &(room, age)) in state.rooms.iter().enumerate() {
                let Some(d) = distances
                    .get(&(room, valve))
                    .and_then(|d| d.checked_sub(age))
                    .filter(|&d| d < state.time) else { continue };
                moves.entry(d).or_insert_with(|| [(); N].map(|_| vec![]))[i].push(valve);
            }
        }
        // if moves
        //     .iter()
        //     .all(|(_, moves)| moves.iter().all(|moves| moves.is_empty()))
        // {
        //     return (None, vec![]);
        // }
        let mut options = vec![];
        for (d, moves) in moves {
            let mut indices = [None; N];
            while indices.iter_mut().enumerate().any(|(i, index)| {
                *index = Some(index.map_or(0, |index| index + 1))
                    .filter(|&index| index < moves[i].len());
                index.is_some()
            }) {
                let mut valves = BTreeSet::new();
                if !indices
                    .iter()
                    .enumerate()
                    .filter_map(|(i, index)| Some((i, index.as_ref()?)))
                    .all(|(i, &index)| valves.insert(moves[i][index]))
                {
                    continue;
                }
                let mut rooms = state.rooms;
                for (i, &index) in indices.iter().enumerate() {
                    match index {
                        Some(index) => rooms[i] = (moves[i][index], 0),
                        _ => rooms[i].1 += d + 1,
                    }
                }
                rooms.sort_unstable();
                let rate = valves.iter().map(|&valve| graph[valve].0).sum::<usize>();
                let new_state = State {
                    rooms,
                    valves: state.valves.difference(&valves).copied().collect(),
                    flow: state.flow + rate,
                    total: state.total + state.flow * (d + 1),
                    time: state.time - d - 1,
                };
                options.push((estimate + rate * new_state.time, new_state));
            }
        }
        (Some(potential), options)
    })
    .fold(0, |acc, state| {
        let value = state.total + state.flow * state.time;
        if value > acc {
            eprintln!("{:?}: {:?}", value, state);
            value
        } else {
            acc
        }
    })
}

pub fn part1<'a, I, S>(lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    solve::<1, 30, _, _>(lines)
}

pub fn part2<'a, I, S>(lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    solve::<2, 26, _, _>(lines)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &[
        "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB",
        "Valve BB has flow rate=13; tunnels lead to valves CC, AA",
        "Valve CC has flow rate=2; tunnels lead to valves DD, BB",
        "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE",
        "Valve EE has flow rate=3; tunnels lead to valves FF, DD",
        "Valve FF has flow rate=0; tunnels lead to valves EE, GG",
        "Valve GG has flow rate=0; tunnels lead to valves FF, HH",
        "Valve HH has flow rate=22; tunnel leads to valve GG",
        "Valve II has flow rate=0; tunnels lead to valves AA, JJ",
        "Valve JJ has flow rate=21; tunnel leads to valve II",
    ];

    #[test]
    fn part1_examples() {
        assert_eq!(1651, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(1707, part2(EXAMPLE));
    }
}
