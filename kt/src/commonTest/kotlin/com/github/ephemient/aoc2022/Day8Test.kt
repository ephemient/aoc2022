package com.github.ephemient.aoc2022

import kotlin.test.Test
import kotlin.test.assertEquals

class Day8Test {
    @Test
    fun part1() {
        assertEquals(21, Day8(SAMPLE_INPUT).part1())
    }

    @Test
    fun part2() {
        assertEquals(8, Day8(SAMPLE_INPUT).part2())
    }

    companion object {
        private val SAMPLE_INPUT = listOf("30373", "25512", "65332", "33549", "35390")
    }
}
