package com.github.ephemient.aoc2022

import kotlin.test.Test
import kotlin.test.assertEquals

class Day9Test {
    @Test
    fun part1() {
        assertEquals(13, Day9(SAMPLE_INPUT_1).part1())
    }

    @Test
    fun part2() {
        assertEquals(1, Day9(SAMPLE_INPUT_1).part2())
        assertEquals(36, Day9(SAMPLE_INPUT_2).part2())
    }

    companion object {
        private val SAMPLE_INPUT_1 = listOf("R 4", "U 4", "L 3", "D 1", "R 4", "D 1", "L 5", "R 2")
        private val SAMPLE_INPUT_2 = listOf("R 5", "U 8", "L 8", "D 3", "R 17", "D 10", "L 25", "U 20")
    }
}
