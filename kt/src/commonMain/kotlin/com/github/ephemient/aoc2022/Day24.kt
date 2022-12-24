package com.github.ephemient.aoc2022

import kotlin.math.abs

@Day
class Day24(private val lines: List<String>) {
    private val start = lines.first().indexOf('.') to 0
    private val end = lines.last().lastIndexOf('.') to lines.lastIndex

    @Suppress("ComplexCondition")
    private fun isFree(x: Int, y: Int, time: Int): Boolean {
        if (x <= 0 || y <= 0 || y >= lines.lastIndex || x >= lines.getOrElse(y) { "" }.lastIndex) {
            return (lines.getOrNull(y)?.getOrNull(x) ?: '#') == '.'
        }
        return lines[y][(x - 1 + time).mod(lines[y].length - 2) + 1] != '<' &&
            lines[y][(x - 1 - time).mod(lines[y].length - 2) + 1] != '>' &&
            lines[(y - 1 + time).mod(lines.size - 2) + 1][x] != '^' &&
            lines[(y - 1 - time).mod(lines.size - 2) + 1][x] != 'v'
    }

    private fun search(start: IntPair, end: IntPair, startTime: Int = 0): Int {
        val (endX, endY) = end
        val seen = mutableSetOf(IndexedValue(startTime, start))
        val queue = PriorityQueue(compareBy(IndexedValue<IndexedValue<IntPair>>::index))
        queue.add(IndexedValue(0, IndexedValue(startTime, start)))
        while (!queue.isEmpty()) {
            val entry = queue.remove().value
            val (time, position) = entry
            if (position == end) return time
            val (x, y) = position
            for ((x2, y2) in arrayOf(x - 1 to y, x to y - 1, x to y, x to y + 1, x + 1 to y)) {
                if (!isFree(x2, y2, time + 1)) continue
                val state = IndexedValue(time + 1, x2 to y2)
                if (seen.add(state)) queue.add(IndexedValue(time + abs(x2 - endX) + abs(y2 - endY), state))
            }
        }
        throw NoSuchElementException()
    }

    @Day.Part
    fun part1(): Int = search(start, end)

    @Day.Part
    fun part2(): Int = search(start, end, search(end, start, search(start, end)))
}
