#[macro_use]
extern crate build_const;

use aoc2022::{
    day1, day10, day11, day12, day13, day13_fast, day14, day2, day3, day4, day5, day6, day7, day8,
    day9,
};
use criterion::{black_box, criterion_group, criterion_main, Criterion};

build_const!("aoc2022");

fn aoc2022_bench(c: &mut Criterion) {
    let mut g = c.benchmark_group("day 1");
    g.bench_function("part 1", |b| b.iter(|| day1::part1(black_box(DAY1))));
    g.bench_function("part 2", |b| b.iter(|| day1::part2(black_box(DAY1))));
    g.finish();
    let mut g = c.benchmark_group("day 2");
    g.bench_function("part 1", |b| b.iter(|| day2::part1(black_box(DAY2))));
    g.bench_function("part 2", |b| b.iter(|| day2::part2(black_box(DAY2))));
    g.finish();
    let mut g = c.benchmark_group("day 3");
    g.bench_function("part 1", |b| b.iter(|| day3::part1(black_box(DAY3))));
    g.bench_function("part 2", |b| b.iter(|| day3::part2(black_box(DAY3))));
    g.finish();
    let mut g = c.benchmark_group("day 4");
    g.bench_function("part 1", |b| b.iter(|| day4::part1(black_box(DAY4))));
    g.bench_function("part 2", |b| b.iter(|| day4::part2(black_box(DAY4))));
    g.finish();
    let mut g = c.benchmark_group("day 5");
    g.bench_function("part 1", |b| b.iter(|| day5::part1(black_box(DAY5))));
    g.bench_function("part 2", |b| b.iter(|| day5::part2(black_box(DAY5))));
    g.finish();
    let mut g = c.benchmark_group("day 6");
    g.bench_function("part 1", |b| b.iter(|| day6::part1(black_box(DAY6))));
    g.bench_function("part 2", |b| b.iter(|| day6::part2(black_box(DAY6))));
    g.finish();
    let mut g = c.benchmark_group("day 7");
    g.bench_function("part 1", |b| b.iter(|| day7::part1(black_box(DAY7))));
    g.bench_function("part 2", |b| b.iter(|| day7::part2(black_box(DAY7))));
    g.finish();
    let mut g = c.benchmark_group("day 8");
    g.bench_function("part 1", |b| b.iter(|| day8::part1(black_box(DAY8))));
    g.bench_function("part 2", |b| b.iter(|| day8::part2(black_box(DAY8))));
    g.finish();
    let mut g = c.benchmark_group("day 9");
    g.bench_function("part 1", |b| b.iter(|| day9::part1(black_box(DAY9))));
    g.bench_function("part 2", |b| b.iter(|| day9::part2(black_box(DAY9))));
    g.finish();
    let mut g = c.benchmark_group("day 10");
    g.bench_function("part 1", |b| b.iter(|| day10::part1(black_box(DAY10))));
    g.bench_function("part 2", |b| b.iter(|| day10::part2(black_box(DAY10))));
    g.finish();
    let mut g = c.benchmark_group("day 11");
    g.bench_function("part 1", |b| b.iter(|| day11::part1(black_box(DAY11))));
    g.bench_function("part 2", |b| b.iter(|| day11::part2(black_box(DAY11))));
    g.finish();
    let mut g = c.benchmark_group("day 12");
    g.bench_function("both parts", |b| {
        b.iter(|| day12::both_parts(black_box(DAY12)))
    });
    g.finish();
    let mut g = c.benchmark_group("day 13");
    g.bench_function("part 1", |b| b.iter(|| day13::part1(black_box(DAY13))));
    g.bench_function("part 1 (fast)", |b| {
        b.iter(|| day13_fast::part1(black_box(DAY13)))
    });
    g.bench_function("part 2", |b| b.iter(|| day13::part2(black_box(DAY13))));
    g.bench_function("part 2 (fast)", |b| {
        b.iter(|| day13_fast::part2(black_box(DAY13)))
    });
    g.finish();
    let mut g = c.benchmark_group("day 14");
    g.bench_function("both", |b| b.iter(|| day14::both_parts(black_box(DAY14))));
    g.finish();
}

criterion_group!(aoc2022, aoc2022_bench);
criterion_main!(aoc2022);
