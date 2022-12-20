package com.github.ephemient.aoc2022

import kotlin.test.Test
import kotlin.test.assertEquals

class Day20Test {
    @Test
    fun part1() {
        assertEquals(3, Day20(getTestInput(20)).part1())
    }

    @Test
    fun part2() {
        assertEquals(1623178306, Day20(getTestInput(20)).part2())
    }
}
