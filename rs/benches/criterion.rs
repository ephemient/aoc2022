#[macro_use]
extern crate build_const;

use aoc2022::{day1, day2, day3, day4, day5, day6};
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
}

criterion_group!(aoc2022, aoc2022_bench);
criterion_main!(aoc2022);
