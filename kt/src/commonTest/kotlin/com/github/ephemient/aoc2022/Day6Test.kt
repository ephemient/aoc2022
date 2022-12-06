package com.github.ephemient.aoc2022

import kotlin.test.Test
import kotlin.test.assertEquals

class Day6Test {
    @Test
    fun part1() {
        assertEquals(7, Day6(listOf("mjqjpqmgbljsphdztnvjfqwrcgsmlb")).part1())
        assertEquals(5, Day6(listOf("bvwbjplbgvbhsrlpgdmjqwftvncz")).part1())
        assertEquals(6, Day6(listOf("nppdvjthqldpwncqszvftbrmjlhg")).part1())
        assertEquals(10, Day6(listOf("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")).part1())
        assertEquals(11, Day6(listOf("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")).part1())
    }

    @Test
    fun part2() {
        assertEquals(19, Day6(listOf("mjqjpqmgbljsphdztnvjfqwrcgsmlb")).part2())
        assertEquals(23, Day6(listOf("bvwbjplbgvbhsrlpgdmjqwftvncz")).part2())
        assertEquals(23, Day6(listOf("nppdvjthqldpwncqszvftbrmjlhg")).part2())
        assertEquals(29, Day6(listOf("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")).part2())
        assertEquals(26, Day6(listOf("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")).part2())
    }
}
