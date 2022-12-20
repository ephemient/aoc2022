package com.github.ephemient.aoc2022

import kotlin.test.Test
import kotlin.test.assertEquals

class Day7Test {
    @Test
    fun part1() {
        assertEquals(95437, Day7(getTestInput(7)).part1())
    }

    @Test
    fun part2() {
        assertEquals(24933642, Day7(getTestInput(7)).part2())
    }
}
