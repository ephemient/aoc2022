package com.github.ephemient.aoc2022

import kotlin.test.Test
import kotlin.test.assertEquals

class Day13Test {
    @Test
    fun part1() {
        assertEquals(13, Day13(SAMPLE_INPUT).part1())
    }

    @Test
    fun part2() {
        assertEquals(140, Day13(SAMPLE_INPUT).part2())
    }

    @Test
    fun part1Fast() {
        assertEquals(13, Day13Fast(SAMPLE_INPUT).part1())
    }

    @Test
    fun part2Fast() {
        assertEquals(140, Day13Fast(SAMPLE_INPUT).part2())
    }

    companion object {
        private val SAMPLE_INPUT = listOf(
            "[1,1,3,1,1]",
            "[1,1,5,1,1]",
            "",
            "[[1],[2,3,4]]",
            "[[1],4]",
            "",
            "[9]",
            "[[8,7,6]]",
            "",
            "[[4,4],4,4]",
            "[[4,4],4,4,4]",
            "",
            "[7,7,7,7]",
            "[7,7,7]",
            "",
            "[]",
            "[3]",
            "",
            "[[[]]]",
            "[[]]",
            "",
            "[1,[2,[3,[4,[5,6,7]]]],8,9]",
            "[1,[2,[3,[4,[5,6,0]]]],8,9]"
        )
    }
}
