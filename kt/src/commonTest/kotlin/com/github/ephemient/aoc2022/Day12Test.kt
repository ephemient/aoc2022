package com.github.ephemient.aoc2022

import kotlin.test.Test
import kotlin.test.assertEquals

class Day12Test {
    @Test
    fun part1() {
        assertEquals(31, Day12(getTestInput(12)).part1())
    }

    @Test
    fun part2() {
        assertEquals(29, Day12(getTestInput(12)).part2())
    }
}
