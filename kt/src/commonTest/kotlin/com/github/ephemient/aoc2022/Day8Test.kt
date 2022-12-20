package com.github.ephemient.aoc2022

import kotlin.test.Test
import kotlin.test.assertEquals

class Day8Test {
    @Test
    fun part1() {
        assertEquals(21, Day8(getTestInput(8)).part1())
    }

    @Test
    fun part2() {
        assertEquals(8, Day8(getTestInput(8)).part2())
    }
}
