#[macro_use]
extern crate build_const;

use aoc2022::{day1, day2};
use std::collections::HashSet;
use std::env;
use std::io;

build_const!("aoc2022");

fn main() -> io::Result<()> {
    let args = env::args()
        .skip(1)
        .filter_map(|s| s.parse().ok())
        .collect::<HashSet<u32>>();

    if args.is_empty() || args.contains(&1) {
        println!("Day 1");
        println!("{:?}", day1::part1(DAY1));
        println!("{:?}", day1::part2(DAY1));
        println!();
    }

    if args.is_empty() || args.contains(&2) {
        println!("Day 2");
        println!("{:?}", day2::part1(DAY2));
        println!("{:?}", day2::part2(DAY2));
        println!();
    }

    Ok(())
}
