package com.github.ephemient.aoc2022

import kotlin.test.Test
import kotlin.test.assertEquals

class Day14Test {
    @Test
    fun part1() {
        assertEquals(24, Day14(getTestInput(14)).part1())
    }

    @Test
    fun part2() {
        assertEquals(93, Day14(getTestInput(14)).part2())
    }
}
