package com.github.ephemient.aoc2022

import kotlin.math.absoluteValue
import kotlin.math.sign

@Day
class Day9(lines: List<String>) {
    private val moves = sequence {
        var x = 0
        var y = 0
        yield(IntPair(x, y))
        for (line in lines) {
            repeat(line.drop(2).toInt()) {
                when (line[0]) {
                    'L' -> x--
                    'R' -> x++
                    'U' -> y--
                    'D' -> y++
                }
                yield(IntPair(x, y))
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

private fun Sequence<IntPair>.follow(): Sequence<IntPair> = scan(IntPair(0, 0)) { tail, (headX, headY) ->
    val dx = headX - tail.first
    val dy = headY - tail.second
    when {
        dx.absoluteValue <= 1 && dy.absoluteValue <= 1 -> tail
        dx.absoluteValue < dy.absoluteValue -> IntPair(headX, headY - dy.sign)
        dx.absoluteValue > dy.absoluteValue -> IntPair(headX - dx.sign, headY)
        else -> IntPair(headX - dx.sign, headY - dy.sign)
    }
}
