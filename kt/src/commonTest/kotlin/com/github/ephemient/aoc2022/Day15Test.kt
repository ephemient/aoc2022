package com.github.ephemient.aoc2022

import kotlin.test.Test
import kotlin.test.assertEquals

class Day15Test {
    @Test
    fun part1() {
        assertEquals(26, Day15(getTestInput(15)).part1())
    }

    @Test
    fun part2() {
        assertEquals(56000011, Day15(getTestInput(15)).part2())
    }
}
