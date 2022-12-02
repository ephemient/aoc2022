package com.github.ephemient.aoc2022

import kotlin.test.Test
import kotlin.test.assertEquals

class Day2Test {
    @Test
    fun part1() {
        assertEquals(15, Day2(SAMPLE_INPUT).part1())
    }

    @Test
    fun part2() {
        assertEquals(12, Day2(SAMPLE_INPUT).part2())
    }

    @Test
    fun sanity() {
        assertEquals(0, Day2(listOf()).part1())
        assertEquals(1, Day2(listOf("B X")).part1())
        assertEquals(2, Day2(listOf("C Y")).part1())
        assertEquals(3, Day2(listOf("A Z")).part1())
        assertEquals(4, Day2(listOf("A X")).part1())
        assertEquals(5, Day2(listOf("B Y")).part1())
        assertEquals(6, Day2(listOf("C Z")).part1())
        assertEquals(7, Day2(listOf("C X")).part1())
        assertEquals(8, Day2(listOf("A Y")).part1())
        assertEquals(9, Day2(listOf("B Z")).part1())
        assertEquals(0, Day2(listOf()).part2())
        assertEquals(1, Day2(listOf("B X")).part2())
        assertEquals(2, Day2(listOf("C X")).part2())
        assertEquals(3, Day2(listOf("A X")).part2())
        assertEquals(4, Day2(listOf("A Y")).part2())
        assertEquals(5, Day2(listOf("B Y")).part2())
        assertEquals(6, Day2(listOf("C Y")).part2())
        assertEquals(7, Day2(listOf("C Z")).part2())
        assertEquals(8, Day2(listOf("A Z")).part2())
        assertEquals(9, Day2(listOf("B Z")).part2())
    }

    companion object {
        private val SAMPLE_INPUT = listOf("A Y", "B X", "C Z")
    }
}
