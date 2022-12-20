package com.github.ephemient.aoc2022

import kotlin.test.Test
import kotlin.test.assertEquals

class Day1Test {
    @Test
    fun part1() {
        assertEquals(24000, Day1(getTestInput(1)).part1())
    }

    @Test
    fun part2() {
        assertEquals(45000, Day1(getTestInput(1)).part2())
    }
}
