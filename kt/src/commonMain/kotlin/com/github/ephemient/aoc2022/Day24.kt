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
        val queue = PriorityQueue(compareBy(IndexedValue<IndexedValue<IntPair>>::index))
        queue.add(IndexedValue(0, IndexedValue(startTime, start)))
        val seen = mutableSetOf<IndexedValue<IntPair>>()
        while (!queue.isEmpty()) {
            val entry = queue.remove().value
            if (!seen.add(entry)) continue
            val (time, position) = entry
            if (position == end) return time
            val (x, y) = position
            if (isFree(x, y, time + 1)) {
                queue.add(IndexedValue(time + abs(x - endX) + abs(y - endY), IndexedValue(time + 1, x to y)))
            }
            if (isFree(x - 1, y, time + 1)) {
                queue.add(IndexedValue(time + abs(x - 1 - endX) + abs(y - endY), IndexedValue(time + 1, x - 1 to y)))
            }
            if (isFree(x + 1, y, time + 1)) {
                queue.add(IndexedValue(time + abs(x + 1 - endX) + abs(y - endY), IndexedValue(time + 1, x + 1 to y)))
            }
            if (isFree(x, y - 1, time + 1)) {
                queue.add(IndexedValue(time + abs(x - endX) + abs(y - 1 - endY), IndexedValue(time + 1, x to y - 1)))
            }
            if (isFree(x, y + 1, time + 1)) {
                queue.add(IndexedValue(time + abs(x - endX) + abs(y + 1 - endY), IndexedValue(time + 1, x to y + 1)))
            }
        }
        throw NoSuchElementException()
    }

    @Day.Part
    fun part1(): Int = search(start, end)

    @Day.Part
    fun part2(): Int = search(start, end, search(end, start, search(start, end)))
}
