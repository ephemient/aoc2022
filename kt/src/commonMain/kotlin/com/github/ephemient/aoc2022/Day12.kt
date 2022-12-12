package com.github.ephemient.aoc2022

@Day
class Day12(private val lines: List<String>) {
    @Day.Part
    fun part1(): Int? = bfs(locationOf('S')) { from -> from.filteredNeighbors { to -> canMove(from, to) } }
        .firstOrNull { (_, value) -> lines[value.first][value.second] == 'E' }
        ?.index

    @Day.Part
    fun part2(): Int? = bfs(locationOf('E')) { to -> to.filteredNeighbors { from -> canMove(from, to) } }
        .firstOrNull { (_, value) -> lines[value.first][value.second] == 'a' }
        ?.index

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
        return if (a == 'S') b == 'a' else if (b == 'E') a == 'z' else b - a <= 1
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
