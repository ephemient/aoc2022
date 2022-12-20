package com.github.ephemient.aoc2022

import kotlin.test.Test
import kotlin.test.assertEquals

class Day13Test {
    @Test
    fun part1() {
        assertEquals(13, Day13(getTestInput(13)).part1())
    }

    @Test
    fun part2() {
        assertEquals(140, Day13(getTestInput(13)).part2())
    }

    @Test
    fun part1Fast() {
        assertEquals(13, Day13Fast(getTestInput(13)).part1())
    }

    @Test
    fun part2Fast() {
        assertEquals(140, Day13Fast(getTestInput(13)).part2())
    }
}
