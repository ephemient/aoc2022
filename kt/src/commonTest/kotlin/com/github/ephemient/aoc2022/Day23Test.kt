package com.github.ephemient.aoc2022

import kotlin.test.Test
import kotlin.test.assertEquals

class Day23Test {
    @Test
    fun part1() {
        assertEquals(110, Day23(getTestInput(23)).part1())
    }

    @Test
    fun part2() {
        assertEquals(20, Day23(getTestInput(23)).part2())
    }
}
