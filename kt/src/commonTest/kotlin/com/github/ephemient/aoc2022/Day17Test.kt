package com.github.ephemient.aoc2022

import kotlin.test.Test
import kotlin.test.assertEquals

class Day17Test {
    @Test
    fun part1() {
        assertEquals(3068, Day17(getTestInput(17)).part1())
    }

    @Test
    fun part2() {
        assertEquals(1514285714288, Day17(getTestInput(17)).part2())
    }
}
