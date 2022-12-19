package com.github.ephemient.aoc2022

import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.async
import kotlinx.coroutines.withContext

@Day
class Day19(lines: List<String>) {
    private val blueprints = lines.map { line ->
        val (id) = ID_PATTERN.matchAt(line, 0)!!.destructured
        id.toInt() to PART_PATTERN.findAll(line).associate { partMatch ->
            val (robot, costs) = partMatch.destructured
            robot to COST_PATTERN.findAll(costs).associate { costMatch ->
                val (cost, type) = costMatch.destructured
                type to cost.toInt()
            }
        }
    }

    @Day.Part
    suspend fun part1(): Int = withContext(Dispatchers.Default) {
        blueprints.map { (id, blueprint) ->
            async {
                id * geodes(blueprint, 24).also { println("($id, $it)") }
            }
        }.sumOf { it.await() }
    }

    @Day.Part
    suspend fun part2(): Int = withContext(Dispatchers.Default) {
        blueprints.take(3).map { (id, blueprint) ->
            async {
                geodes(blueprint, 32).also { println("($id, $it)") }
            }
        }.fold(1) { acc, deferred -> acc * deferred.await() }
    }

    private data class State(
        val robots: Map<String, Int>,
        val resources: Map<String, Int>,
        val time: Int,
    ) {
        val estimate: Int = resources.get0("geode") + robots.get0("geode") * time
    }

    companion object {
        private val ID_PATTERN = """Blueprint (\d+):""".toRegex()
        private val PART_PATTERN = """Each (\w+) robot ([^.]+)[.]""".toRegex()
        private val COST_PATTERN = """(?:costs | and )(\d+) (\w+)""".toRegex()

        private fun <T> Map<T, Int>.get0(key: T): Int = getOrElse(key) { 0 }

        private fun geodes(blueprint: Map<String, Map<String, Int>>, time: Int): Int {
            var best = 0
            val queue = mutableListOf(State(mapOf("ore" to 1), emptyMap(), time))
            while (queue.isNotEmpty()) {
                val state = queue.removeLast()
                if (potential(blueprint, state) < best) continue
                if (state.estimate > best) best = state.estimate
                for ((robot, costs) in blueprint) {
                    val delta = blueprint.keys.maxOf { type ->
                        val demand = costs.get0(type) - state.resources.get0(type)
                        if (demand <= 0) {
                            0
                        } else {
                            val supply = state.robots.get0(type)
                            if (supply <= 0) Int.MAX_VALUE else (demand + supply - 1) / supply
                        }
                    }
                    if (delta < state.time) {
                        val robots = state.robots + (robot to state.robots.get0(robot) + 1)
                        val resources = buildMap {
                            putAll(state.resources)
                            for ((type, cost) in costs) this[type] = this.get0(type) - cost
                            for ((type, count) in state.robots) this[type] = this.get0(type) + count * (delta + 1)
                        }
                        queue.add(State(robots, resources, state.time - delta - 1))
                    }
                }
            }
            return best
        }

        private fun potential(blueprint: Map<String, Map<String, Int>>, state: State): Int {
            val potentialRobots = blueprint.keys.associateWithTo(mutableMapOf()) { 0 }
            val potentialResources = state.resources.toMutableMap()
            repeat(state.time) {
                for ((robot, count) in potentialRobots) {
                    potentialResources[robot] = potentialResources.get0(robot) + state.robots.get0(robot) + count
                }
                for (entry in potentialRobots) {
                    val (robot, count) = entry
                    if (
                        blueprint[robot]!!.all { (type, cost) ->
                            potentialResources.get0(type) >= cost * (count + 1)
                        }
                    ) {
                        entry.setValue(count + 1)
                    }
                }
            }
            return potentialResources.get0("geode")
        }
    }
}
