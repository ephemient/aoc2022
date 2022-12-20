package com.github.ephemient.aoc2022

import kotlin.test.Test
import kotlin.test.assertEquals

class Day20Test {
    @Test
    fun part1() {
        assertEquals(3, Day20(SAMPLE_INPUT).part1())
    }

    @Test
    fun part2() {
        assertEquals(1623178306, Day20(SAMPLE_INPUT).part2())
    }

    companion object {
        private val SAMPLE_INPUT = listOf("1", "2", "-3", "3", "-2", "0", "4")
    }
}
