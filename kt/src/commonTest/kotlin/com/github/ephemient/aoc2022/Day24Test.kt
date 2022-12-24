package com.github.ephemient.aoc2022

import kotlin.test.Test
import kotlin.test.assertEquals

class Day24Test {
    @Test
    fun part1() {
        assertEquals(18, Day24(getTestInput(24)).part1())
    }

    @Test
    fun part2() {
        assertEquals(54, Day24(getTestInput(24)).part2())
    }
}
