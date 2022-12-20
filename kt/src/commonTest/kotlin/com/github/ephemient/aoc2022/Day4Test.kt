package com.github.ephemient.aoc2022

import kotlin.test.Test
import kotlin.test.assertEquals

class Day4Test {
    @Test
    fun part1() {
        assertEquals(2, Day4(getTestInput(4)).part1())
    }

    @Test
    fun part2() {
        assertEquals(4, Day4(getTestInput(4)).part2())
    }
}
