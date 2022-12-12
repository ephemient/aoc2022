package com.github.ephemient.aoc2022

@Day
class Day12(private val lines: List<String>) {
    private val part1: Int?
    private val part2: Int?
    init {
        var part1: Int? = null
        var part2: Int? = null
        @Suppress("LoopWithTooManyJumpStatements")
        for ((value, point) in bfs(locationOf('E')) { to -> to.filteredNeighbors { from -> canMove(from, to) } }) {
            when (lines[point.first][point.second]) {
                'S' -> part1 = part1 ?: value
                'a' -> part2 = part2 ?: value
                else -> continue
            }
            if (part1 != null && part2 != null) break
        }
        this.part1 = part1
        this.part2 = part2
    }

    @Day.Part
    fun part1(): Int? = part1

    @Day.Part
    fun part2(): Int? = part2

    private fun locationOf(char: Char): IntPair {
        lines.forEachIndexed { y, line ->
            val x = line.indexOf(char)
            if (x >= 0) return y to x
        }
        throw NoSuchElementException()
    }

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
