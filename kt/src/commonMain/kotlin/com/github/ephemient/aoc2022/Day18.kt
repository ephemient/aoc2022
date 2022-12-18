package com.github.ephemient.aoc2022

@Day
class Day18(lines: List<String>) {
    private val points = buildSet {
        lines.mapTo(this) { line ->
            val (x, y, z) = line.split(',', limit = 3)
            IntTriple(x.toInt(), y.toInt(), z.toInt())
        }
    }

    @Day.Part
    fun part1(): Int = points.sumOf { point -> point.neighbors().count { it !in points } }

    @Day.Part
    fun part2(): Int {
        var minX = Int.MAX_VALUE
        var maxX = Int.MIN_VALUE
        var minY = Int.MAX_VALUE
        var maxY = Int.MIN_VALUE
        var minZ = Int.MAX_VALUE
        var maxZ = Int.MIN_VALUE
        for ((x, y, z) in points) {
            if (x < minX) minX = x
            if (x > maxX) maxX = x
            if (y < minY) minY = y
            if (y > maxY) maxY = y
            if (z < minZ) minZ = z
            if (z > maxZ) maxZ = z
        }
        val outside = buildSet {
            val queue = mutableListOf(IntTriple(minX - 1, minY - 1, minZ - 1).also { add(it) })
            while (queue.isNotEmpty()) {
                for (neighbor in queue.removeLast().neighbors()) {
                    neighbor.first in minX - 1..maxX + 1 &&
                        neighbor.second in minY - 1..maxY + 1 &&
                        neighbor.third in minZ - 1..maxZ + 1 &&
                        neighbor !in points &&
                        add(neighbor) &&
                        queue.add(neighbor)
                }
            }
        }
        return points.sumOf { point -> point.neighbors().count { it in outside } }
    }
}

private fun IntTriple.neighbors() = listOf(
    copy(first = first - 1),
    copy(first = first + 1),
    copy(second = second - 1),
    copy(second = second + 1),
    copy(third = third - 1),
    copy(third = third + 1),
)
