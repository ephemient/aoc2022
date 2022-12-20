package com.github.ephemient.aoc2022

import kotlin.test.Test
import kotlin.test.assertEquals

class Day18Test {
    @Test
    fun part1() {
        assertEquals(64, Day18(getTestInput(18)).part1())
    }

    @Test
    fun part2() {
        assertEquals(58, Day18(getTestInput(18)).part2())
    }
}
