package com.github.ephemient.aoc2022

import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day4Bench {
    private lateinit var lines: List<String>

    @Setup
    fun prepare() {
        lines = getInput(4)
    }

    @Benchmark
    fun part1(): Int = Day4(lines).part1()

    @Benchmark
    fun part2(): Int = Day4(lines).part2()
}
