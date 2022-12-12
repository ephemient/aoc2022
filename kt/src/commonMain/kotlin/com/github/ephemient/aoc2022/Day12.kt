package com.github.ephemient.aoc2022

@Day
class Day12(private val lines: List<String>) {
    private val result = bfs(
        lines.withIndex().firstNotNullOf { (y, line) -> line.indexOf('E').takeIf { it >= 0 }?.let { x -> y to x } }
    ) { to -> to.filteredNeighbors { from -> canMove(from, to) } }
        .scan(-1 to -1) { result, (value, point) ->
            when (lines[point.first][point.second]) {
                'S' -> if (result.first < 0) return@scan result.copy(first = value)
                'a' -> if (result.second < 0) return@scan result.copy(second = value)
            }
            result
        }
        .first { it.first >= 0 && it.second >= 0 }

    @Day.Part
    fun part1(): Int = result.first

    @Day.Part
    fun part2(): Int = result.second

    private inline fun IntPair.filteredNeighbors(predicate: (IntPair) -> Boolean): List<IntPair> =
        arrayOf(first - 1 to second, first to second - 1, first to second + 1, first + 1 to second)
            .filter { it.first in lines.indices && it.second in lines[it.first].indices && predicate(it) }

    private fun canMove(from: IntPair, to: IntPair): Boolean {
        val a = lines[from.first][from.second]
        val b = lines[to.first][to.second]
        return b.mappedCode - a.mappedCode <= 1
    }
}

private fun <T> bfs(start: T, next: (T) -> Iterable<T>): Sequence<IndexedValue<T>> = sequence {
    val seen = mutableSetOf(start)
    val queue = ArrayDeque(listOf(IndexedValue(0, start)))
    while (queue.isNotEmpty()) {
        val (i, a) = queue.removeFirst().also { yield(it) }
        for (b in next(a)) if (seen.add(b)) queue.add(IndexedValue(i + 1, b))
    }
}

private val Char.mappedCode: Int
    get() = when (this) {
        'S' -> 'a'
        'E' -> 'z'
        else -> this
    }.code
