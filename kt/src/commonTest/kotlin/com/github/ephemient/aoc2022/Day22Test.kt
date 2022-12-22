package com.github.ephemient.aoc2022

import kotlin.test.Test
import kotlin.test.assertEquals

class Day22Test {
    @Test
    fun part1() {
        assertEquals(6032, Day22(getTestInput(22)).part1())
    }

    @Test
    fun part2() {
        assertEquals(5031, Day22(getTestInput(22)).part2())
    }
}
