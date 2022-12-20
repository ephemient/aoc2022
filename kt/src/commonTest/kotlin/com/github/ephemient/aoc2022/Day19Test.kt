package com.github.ephemient.aoc2022

import kotlinx.coroutines.ExperimentalCoroutinesApi
import kotlinx.coroutines.test.runTest
import kotlin.test.Test
import kotlin.test.assertEquals

@OptIn(ExperimentalCoroutinesApi::class)
class Day19Test {
    @Test
    fun part1() = runTest {
        assertEquals(33, Day19(SAMPLE_INPUT).part1())
    }

    @Test
    fun part2() = runTest {
        assertEquals(3472, Day19(SAMPLE_INPUT).part2())
    }

    companion object {
        private val SAMPLE_INPUT = listOf(
            "Blueprint 1: Each ore robot costs 4 ore. " +
                "Each clay robot costs 2 ore. " +
                "Each obsidian robot costs 3 ore and 14 clay. " +
                "Each geode robot costs 2 ore and 7 obsidian.",
            "Blueprint 2: " +
                "Each ore robot costs 2 ore. " +
                "Each clay robot costs 3 ore. " +
                "Each obsidian robot costs 3 ore and 8 clay. " +
                "Each geode robot costs 3 ore and 12 obsidian."
        )
    }
}
