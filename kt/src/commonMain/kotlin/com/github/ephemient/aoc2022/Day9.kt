package com.github.ephemient.aoc2022

import kotlin.math.absoluteValue
import kotlin.math.sign

@Day
class Day9(lines: List<String>) {
    private val moves = sequence {
        var x = 0
        var y = 0
        yield(x to y)
        for (line in lines) {
            when (line[0]) {
                'L' -> repeat(line.drop(2).toInt()) { yield(--x to y) }
                'R' -> repeat(line.drop(2).toInt()) { yield(++x to y) }
                'U' -> repeat(line.drop(2).toInt()) { yield(x to --y) }
                'D' -> repeat(line.drop(2).toInt()) { yield(x to ++y) }
            }
        }
    }

    @Day.Part
    fun part1(): Int = moves.follow().toSet().size

    @Day.Part
    fun part2(): Int = moves
        .follow() // 1
        .follow() // 2
        .follow() // 3
        .follow() // 4
        .follow() // 5
        .follow() // 6
        .follow() // 7
        .follow() // 8
        .follow() // 9
        .toSet()
        .size
}

private fun Sequence<IntPair>.follow(): Sequence<IntPair> = sequence {
    var tailX = 0
    var tailY = 0
    yield(tailX to tailY)
    for ((headX, headY) in this@follow) {
        val deltaX = headX - tailX
        val deltaY = headY - tailY
        if (deltaX.absoluteValue <= 1 && deltaY.absoluteValue <= 1) continue
        tailX = if (deltaX.absoluteValue >= deltaY.absoluteValue) headX - deltaX.sign else headX
        tailY = if (deltaX.absoluteValue <= deltaY.absoluteValue) headY - deltaY.sign else headY
        yield(tailX to tailY)
    }
}
