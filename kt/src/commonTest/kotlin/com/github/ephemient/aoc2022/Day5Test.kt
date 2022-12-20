package com.github.ephemient.aoc2022

import kotlin.test.Test
import kotlin.test.assertEquals

class Day5Test {
    @Test
    fun part1() {
        assertEquals("CMZ", Day5(getTestInput(5)).part1())
    }

    @Test
    fun part2() {
        assertEquals("MCD", Day5(getTestInput(5)).part2())
    }
}
