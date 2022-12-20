package com.github.ephemient.aoc2022

import kotlin.test.Test
import kotlin.test.assertEquals

class Day3Test {
    @Test
    fun part1() {
        assertEquals(157, Day3(getTestInput(3)).part1())
    }

    @Test
    fun part2() {
        assertEquals(70, Day3(getTestInput(3)).part2())
    }
}
