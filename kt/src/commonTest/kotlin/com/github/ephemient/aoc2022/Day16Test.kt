package com.github.ephemient.aoc2022

import kotlin.test.Test
import kotlin.test.assertEquals

class Day16Test {
    @Test
    fun part1() {
        assertEquals(1651, Day16(getTestInput(16)).part1())
    }

    @Test
    fun part2() {
        assertEquals(1707, Day16(getTestInput(16)).part2())
    }
}
