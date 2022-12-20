package com.github.ephemient.aoc2022

import kotlinx.coroutines.ExperimentalCoroutinesApi
import kotlinx.coroutines.test.runTest
import kotlin.test.Test
import kotlin.test.assertEquals

@OptIn(ExperimentalCoroutinesApi::class)
class Day19Test {
    @Test
    fun part1() = runTest {
        assertEquals(33, Day19(getTestInput(19)).part1())
    }

    @Test
    fun part2() = runTest(dispatchTimeoutMs = 120_000L) {
        assertEquals(3472, Day19(getTestInput(19)).part2())
    }
}