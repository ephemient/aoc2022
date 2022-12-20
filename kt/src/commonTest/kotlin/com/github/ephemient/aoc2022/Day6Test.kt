package com.github.ephemient.aoc2022

import kotlin.test.Test
import kotlin.test.assertEquals

class Day6Test {
    @Test
    fun part1() {
        assertEquals(7, Day6(getTestInput(6)).part1())
        assertEquals(5, Day6(getTestInput(6, "a")).part1())
        assertEquals(6, Day6(getTestInput(6, "b")).part1())
        assertEquals(10, Day6(getTestInput(6, "c")).part1())
        assertEquals(11, Day6(getTestInput(6, "d")).part1())
    }

    @Test
    fun part2() {
        assertEquals(19, Day6(getTestInput(6)).part2())
        assertEquals(23, Day6(getTestInput(6, "a")).part2())
        assertEquals(23, Day6(getTestInput(6, "b")).part2())
        assertEquals(29, Day6(getTestInput(6, "c")).part2())
        assertEquals(26, Day6(getTestInput(6, "d")).part2())
    }
}
