package com.github.ephemient.aoc2022

import kotlin.test.Test
import kotlin.test.assertEquals

class Day11Test {
    @Test
    fun part1() {
        assertEquals(10605, Day11(getTestInput(11)).part1())
    }

    @Test
    fun part2() {
        assertEquals(2713310158, Day11(getTestInput(11)).part2())
    }
}
