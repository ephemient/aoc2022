package com.github.ephemient.aoc2022

@Day
class Day22(lines: List<String>) {
    private val board = lines.take(lines.size - 2)
    private val moves = PATTERN.findAll(lines.last()).mapNotNull { match ->
        match.groups[1]?.value?.toIntOrNull()?.let { Move.Steps(it) }
            ?: match.groups[2]?.let { Move.Left }
            ?: match.groups[3]?.let { Move.Right }
    }.toList()

    private val initialPosition = board[0].indexOf('.') to 0
    private val perimeter = buildList {
        var position = initialPosition
        var direction = 0
        do {
            add(IndexedValue(direction, position))
            val forward = position.move(direction)
            if (board[forward] == ' ') {
                direction = (direction + 1).mod(4)
            } else {
                val left = forward.move(direction - 1)
                if (board[left] == ' ') {
                    position = forward
                } else {
                    position = left
                    direction = (direction - 1).mod(4)
                }
            }
        } while (position != initialPosition || direction != 0)
    }

    @Day.Part
    fun part1(): Int = run(
        buildMap {
            for (entry in perimeter) {
                val direction = (entry.index - 1).mod(4)
                var (x, y) = entry.value
                when (direction) {
                    0 -> x = board[y].indexOfFirst { it != ' ' }
                    1 -> y = board.indexOfFirst { x in it.indices && it[x] != ' ' }
                    2 -> x = board[y].indexOfLast { it != ' ' }
                    3 -> y = board.indexOfLast { x in it.indices && it[x] != ' ' }
                }
                put(entry.copy(index = direction), IndexedValue(direction, x to y))
            }
        }
    )

    @Day.Part
    fun part2(): Int = run(
        buildMap {
            var sideLength = Int.MAX_VALUE
            var previousDirection = 0
            var directionCount = 0
            for ((direction, _) in perimeter) {
                if (direction == previousDirection) {
                    directionCount++
                } else {
                    sideLength = minOf(sideLength, directionCount)
                    previousDirection = direction
                    directionCount = 1
                }
            }
            sideLength = minOf(sideLength, directionCount)
            val unpairedEdges = MutableList(perimeter.size / sideLength) { i ->
                IndexedValue(perimeter[i * sideLength].index, perimeter.subList(i * sideLength, (i + 1) * sideLength))
            }
            while (unpairedEdges.isNotEmpty()) {
                var i = 0
                while (i < unpairedEdges.lastIndex) {
                    val a = unpairedEdges[i]
                    val b = unpairedEdges[i + 1]
                    if ((a.index - b.index).mod(4) == 1) {
                        unpairedEdges.subList(i, i + 2).clear()
                        for (j in i..unpairedEdges.lastIndex) {
                            val edge = unpairedEdges[j]
                            unpairedEdges[j] = edge.copy(index = (edge.index - 1).mod(4))
                        }
                        val edge1 = a.value
                        val edge2 = b.value
                        for (j in 0 until sideLength) {
                            val (direction1, position1) = edge1[j]
                            val (direction2, position2) = edge2[sideLength - j - 1]
                            this[IndexedValue((direction1 - 1).mod(4), position1)] =
                                IndexedValue((direction2 + 1).mod(4), position2)
                            this[IndexedValue((direction2 - 1).mod(4), position2)] =
                                IndexedValue((direction1 + 1).mod(4), position1)
                        }
                    } else {
                        i++
                    }
                }
            }
        }
    )

    private fun run(adjacencies: Map<IndexedValue<IntPair>, IndexedValue<IntPair>>): Int {
        val (direction, position) = moves.fold(IndexedValue(0, initialPosition)) outer@{ state, move ->
            when (move) {
                is Move.Steps -> {
                    (1..move.count).fold(state) { state, _ ->
                        val next = adjacencies.getOrElse(state) { state.copy(value = state.value.move(state.index)) }
                        if (board[next.value] != '.') return@outer state
                        next
                    }
                }
                Move.Left -> state.copy(index = (state.index - 1).mod(4))
                Move.Right -> state.copy(index = (state.index + 1).mod(4))
            }
        }
        return 1000 * (position.second + 1) + 4 * (position.first + 1) + direction
    }

    private sealed class Move {
        data class Steps(val count: Int) : Move()
        object Left : Move() {
            override fun toString(): String = "Left"
        }
        object Right : Move() {
            override fun toString(): String = "Right"
        }
    }

    companion object {
        private val PATTERN = """(\d+)|(L)|(R)""".toRegex()

        private operator fun List<String>.get(position: IntPair): Char =
            getOrNull(position.second)?.getOrNull(position.first) ?: ' '

        private fun IntPair.move(direction: Int): IntPair = when (direction.mod(4)) {
            0 -> first + 1 to second
            1 -> first to second + 1
            2 -> first - 1 to second
            3 -> first to second - 1
            else -> throw AssertionError()
        }
    }
}
