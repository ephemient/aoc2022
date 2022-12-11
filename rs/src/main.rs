#[macro_use]
extern crate build_const;

use aoc2022::{day1, day10, day11, day2, day3, day4, day5, day6, day7, day8, day9};
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

    if args.is_empty() || args.contains(&3) {
        println!("Day 3");
        println!("{:?}", day3::part1(DAY3));
        println!("{:?}", day3::part2(DAY3));
        println!();
    }

    if args.is_empty() || args.contains(&4) {
        println!("Day 4");
        println!("{:?}", day4::part1(DAY4));
        println!("{:?}", day4::part2(DAY4));
        println!();
    }

    if args.is_empty() || args.contains(&5) {
        println!("Day 5");
        println!("{}", day5::part1(DAY5));
        println!("{}", day5::part2(DAY5));
        println!();
    }

    if args.is_empty() || args.contains(&6) {
        println!("Day 6");
        println!("{:?}", day6::part1(DAY6).expect("error"));
        println!("{:?}", day6::part2(DAY6).expect("error"));
        println!();
    }

    if args.is_empty() || args.contains(&7) {
        println!("Day 7");
        println!("{:?}", day7::part1(DAY7));
        println!("{:?}", day7::part2(DAY7));
        println!();
    }

    if args.is_empty() || args.contains(&8) {
        println!("Day 8");
        println!("{:?}", day8::part1(DAY8));
        println!("{:?}", day8::part2(DAY8).expect("error"));
        println!();
    }

    if args.is_empty() || args.contains(&9) {
        println!("Day 9");
        println!("{:?}", day9::part1(DAY9));
        println!("{:?}", day9::part2(DAY9));
        println!();
    }

    if args.is_empty() || args.contains(&10) {
        println!("Day 10");
        println!("{:?}", day10::part1(DAY10));
        println!("{}", day10::part2(DAY10));
        println!();
    }

    if args.is_empty() || args.contains(&11) {
        println!("Day 11");
        println!("{:?}", day11::part1(DAY11));
        println!("{:?}", day11::part2(DAY11));
        println!();
    }

    Ok(())
}
