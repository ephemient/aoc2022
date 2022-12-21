package com.github.ephemient.aoc2022

@Day
class Day16(lines: List<String>) {
    private val graph = lines.associate { line ->
        val (src, rate, dsts) = requireNotNull(PATTERN.matchEntire(line)) { line }.destructured
        src to IndexedValue(rate.toInt(), dsts.split(", "))
    }
    private val distances = run {
        val vertices = buildSet {
            addAll(graph.keys)
            for (value in graph.values) addAll(value.value)
        }
        val vertexIndices = vertices.withIndex().associateBy(keySelector = { it.value }, valueTransform = { it.index })
        val adj = Array(vertices.size) { arrayOfNulls<Int>(vertices.size).apply { set(it, 0) } }
        graph.values.forEachIndexed { i, value -> for (dst in value.value) adj[i][vertexIndices.getValue(dst)] = 1 }
        repeat(vertices.size) { k ->
            repeat(vertices.size) { i ->
                repeat(vertices.size) { j ->
                    val a = adj[i][k]
                    val b = adj[k][j]
                    if (a != null && b != null) adj[i][j] = adj[i][j]?.let { minOf(it, a + b) } ?: (a + b)
                }
            }
        }
        buildMap {
            vertices.forEachIndexed { i, u ->
                this[u] = buildMap {
                    vertices.forEachIndexed { j, v ->
                        adj[i][j]?.let { this[v] = it }
                    }
                }
            }
        }
    }

    @Day.Part
    fun part1(): Int = solve(1, 30)

    @Day.Part
    fun part2(): Int = solve(2, 26)

    @Suppress("CyclomaticComplexMethod", "LongMethod", "LoopWithTooManyJumpStatements", "NestedBlockDepth")
    private fun solve(agents: Int, totalTime: Int): Int {
        var max = 0
        val seen = mutableSetOf<State>()
        val queue = PriorityQueue<IndexedValue<State>>(compareBy(reverseOrder()) { it.index })
        queue.add(
            IndexedValue(
                0,
                State(
                    rooms = List(agents) { IndexedValue(0, "AA") },
                    valves = graph.entries.mapNotNullTo(mutableSetOf()) { (key, value) ->
                        if (value.index > 0) key else null
                    },
                    flow = 0,
                    total = 0,
                    timeRemaining = totalTime,
                )
            )
        )
        while (!queue.isEmpty()) {
            val (estimate, state) = queue.remove()
            if (!seen.add(state)) continue
            val potential = estimate + state.valves.sumOf { valve ->
                state.rooms.maxOfOrNull { (age, room) ->
                    val d = (distances[room]?.get(valve) ?: return@maxOfOrNull 0) - age
                    if (d in 0 until state.timeRemaining) {
                        graph.getValue(valve).index * (state.timeRemaining - d - 1)
                    } else {
                        0
                    }
                } ?: 0
            }
            if (estimate > max) max = estimate
            if (potential < max) continue
            val movesByTime: Map<Int, List<List<String>>> = buildMap<_, MutableList<MutableList<String>>> {
                for (valve in state.valves) {
                    state.rooms.forEachIndexed { i, room ->
                        val d = (distances[room.value]?.get(valve) ?: return@forEachIndexed) - room.index
                        if (d in 0 until state.timeRemaining) {
                            getOrPut(d) { MutableList(agents) { mutableListOf() } }[i].add(valve)
                        }
                    }
                }
            }
            if (movesByTime.values.all { movesByAgent -> movesByAgent.all { it.isEmpty() } }) continue
            for ((d, movesByAgent) in movesByTime) {
                val indices = IntArray(agents) { -1 }
                while (
                    indices.withIndex().any { (i, j) ->
                        (if (j < movesByAgent[i].lastIndex) j + 1 else -1).also { indices[i] = it } >= 0
                    }
                ) {
                    val allUnique: Boolean
                    val valves = buildSet {
                        allUnique = indices.withIndex().all { (i, j) -> j < 0 || add(movesByAgent[i][j]) }
                    }
                    if (allUnique) {
                        val rooms = indices.mapIndexed { i, j ->
                            if (j >= 0) {
                                IndexedValue(0, movesByAgent[i][j])
                            } else {
                                val (age, room) = state.rooms[i]
                                IndexedValue(age + d + 1, room)
                            }
                        }.sortedWith(compareBy({ it.value }, { it.index }))
                        val rate = indices.withIndex().sumOf { (i, j) ->
                            if (j >= 0) graph.getValue(movesByAgent[i][j]).index else 0
                        }
                        val newState = State(
                            rooms = rooms,
                            valves = state.valves - valves,
                            flow = state.flow + rate,
                            total = state.total + state.flow * (d + 1),
                            timeRemaining = state.timeRemaining - d - 1,
                        )
                        queue.add(IndexedValue(estimate + rate * newState.timeRemaining, newState))
                    }
                }
            }
        }
        return max
    }

    private data class State(
        val rooms: List<IndexedValue<String>>,
        val valves: Set<String>,
        val flow: Int,
        val total: Int,
        val timeRemaining: Int,
    )
}

private val PATTERN =
    """Valve (\w+) has flow rate=(\d+); (?:tunnel leads to valve|tunnels lead to valves) (\w+(?:, \w+)*)""".toRegex()
