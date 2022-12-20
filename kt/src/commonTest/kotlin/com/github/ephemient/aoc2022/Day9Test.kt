package com.github.ephemient.aoc2022

import kotlin.test.Test
import kotlin.test.assertEquals

class Day9Test {
    @Test
    fun part1() {
        assertEquals(13, Day9(getTestInput(9)).part1())
    }

    @Test
    fun part2() {
        assertEquals(1, Day9(getTestInput(9)).part2())
        assertEquals(36, Day9(getTestInput(9, "a")).part2())
    }
}
