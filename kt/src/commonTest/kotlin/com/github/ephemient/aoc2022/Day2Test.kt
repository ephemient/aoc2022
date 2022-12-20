package com.github.ephemient.aoc2022

import kotlin.test.Test
import kotlin.test.assertEquals

class Day2Test {
    @Test
    fun part1() {
        assertEquals(15, Day2(getTestInput(2)).part1())
    }

    @Test
    fun part2() {
        assertEquals(12, Day2(getTestInput(2)).part2())
    }
}
