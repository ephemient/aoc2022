package com.github.ephemient.aoc2022

import kotlin.test.Test
import kotlin.test.assertEquals

class Day21Test {
    @Test
    fun part1() {
        assertEquals(152, Day21(getTestInput(21)).part1())
    }

    @Test
    fun part2() {
        assertEquals(301, Day21(getTestInput(21)).part2())
    }
}
