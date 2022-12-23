package com.github.ephemient.aoc2022

@Day
class Day23(lines: List<String>) {
    private val states = directions.scan(
        buildSet { lines.forEachIndexed { y, line -> line.forEachIndexed { x, c -> if (c == '#') add(x to y) } } }
    ) { state, directions ->
        val proposals = state.groupingBy { position ->
            if (position.neighbors().none { it in state }) {
                position
            } else {
                directions.firstNotNullOfOrNull { direction ->
                    if (position.neighbors(direction).none { it in state }) position.move(direction) else null
                } ?: position
            }
        }.aggregateTo(mutableMapOf()) { _, _: IntPair?, value, first -> if (first) value else null }
        proposals.entries.removeAll { (key, value) -> value == null || key == value }
        buildSet(state.size) {
            addAll(state)
            removeAll(proposals.values.toSet())
            addAll(proposals.keys)
        }
    }

    @Day.Part
    fun part1(): Int {
        val state = states.elementAt(10)
        val minX = state.minOf { it.first }
        val maxX = state.maxOf { it.first }
        val minY = state.minOf { it.second }
        val maxY = state.maxOf { it.second }
        return (maxX - minX + 1) * (maxY - minY + 1) - state.size
    }

    @Day.Part
    fun part2(): Int = states.zipWithNext().indexOfFirst { (prev, cur) -> prev == cur } + 1

    private enum class Direction {
        N, S, W, E
    }

    companion object {
        private val directions = sequence {
            val directions = ArrayDeque(Direction.values().asList())
            while (true) {
                yield(directions.toList())
                directions.add(directions.removeFirst())
            }
        }

        private fun IntPair.neighbors(): Array<IntPair> = arrayOf(
            first + 1 to second,
            first + 1 to second + 1,
            first to second + 1,
            first - 1 to second + 1,
            first - 1 to second,
            first - 1 to second - 1,
            first to second - 1,
            first + 1 to second - 1,
        )

        private fun IntPair.neighbors(direction: Direction): Array<IntPair> = when (direction) {
            Direction.N -> arrayOf(first - 1 to second - 1, first to second - 1, first + 1 to second - 1)
            Direction.S -> arrayOf(first - 1 to second + 1, first to second + 1, first + 1 to second + 1)
            Direction.W -> arrayOf(first - 1 to second - 1, first - 1 to second, first - 1 to second + 1)
            Direction.E -> arrayOf(first + 1 to second - 1, first + 1 to second, first + 1 to second + 1)
        }

        private fun IntPair.move(direction: Direction) = when (direction) {
            Direction.N -> first to second - 1
            Direction.S -> first to second + 1
            Direction.W -> first - 1 to second
            Direction.E -> first + 1 to second
        }
    }
}
